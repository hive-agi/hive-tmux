#!/usr/bin/env python3
"""A scripted stand-in for a Claude Code 2.1.330 pane held busy by a /goal
Stop-hook loop, for the e2e test compact_goal_loop_e2e_test.clj (card
20261122170735-7c6f5ad3). Run it under `exec -a claude` so tmux reports
pane_current_command `claude`.

Behaviour, as measured on a scratch Claude Code 2.1.330 pane, 2026-11-22:
  * while the goal is active the spinner never goes away: a turn runs, the
    spinner says `(running Stop hooks... 1/2 ...)`, the evaluator line
    `Goal not yet met... continuing` is drawn, the next turn starts;
  * a /compact submitted while busy is Queued and stays queued;
  * `/goal clear` is taken in at once, even mid-turn; the running turn then
    ends, the pane rests and the queued /compact runs;
  * /compact at rest compacts: the context drops and a compact_boundary
    line lands in the transcript;
  * /goal <text> sets the goal (a /goal entry in the transcript) and starts
    the loop again; any other prompt runs one turn.

argv: TRANSCRIPT_PATH TOKENS GOAL_TEXT. Every submitted line is appended to
TRANSCRIPT_PATH + ".sent" so the test can read what reached the pane.
"""
import json
import os
import select
import sys
import termios
import time
import tty

TRANSCRIPT, TOKENS, GOAL = sys.argv[1], int(sys.argv[2]), sys.argv[3]
SENT = TRANSCRIPT + ".sent"
TICK = 0.4          # seconds per animation step
TURN_STEPS = 4      # steps a turn runs
HOOK_STEPS = 2      # steps the Stop-hook spinner shows
CONT_STEPS = 2      # steps the evaluator line shows before the next turn
COMPACT_STEPS = 3

state = {"goal": True, "phase": "turn", "left": TURN_STEPS, "tokens": TOKENS,
         "queued": [], "typed": "", "elapsed": 0, "continuing": False, "last": "tick"}


def append(path, obj):
    with open(path, "a") as f:
        f.write((obj if isinstance(obj, str) else json.dumps(obj)) + "\n")


def goal_entry(args):
    return {"type": "user", "message": {"role": "user", "content":
            "<command-name>/goal</command-name>\n<command-message>goal</command-message>\n"
            "<command-args>" + args + "</command-args>"}}


def draw():
    s = state
    out = ["\x1b[H\x1b[2J", "● " + s["last"] + "\r\n"]
    if s["continuing"] and s["goal"]:
        out.append("◯ Goal not yet met… continuing\r\n")
        out.append("  Goal: " + GOAL[:100] + "\r\n")
    for q in s["queued"]:
        out.append("❯ " + q + "\r\n")
    if s["queued"]:
        out.append("  ⎿  Queued · ctrl+x ctrl+s to send now\r\n")
    t = "%ds" % s["elapsed"]
    if s["phase"] == "compact":
        out.append("✢ Compacting conversation… (1s · ↓ 211 tokens)\r\n")
    elif s["phase"] == "hook":
        out.append("* Pouncing… (running Stop hooks… 1/2 · " + t + " · ↓ 2.0k tokens)\r\n")
    elif s["phase"] == "rest":
        out.append("✻ Crunched for " + t + "\r\n")
    else:
        out.append("✻ Pouncing… (" + t + " · ↓ 2.0k tokens)\r\n")
    out.append("   %d tokens%s\r\n" % (s["tokens"], " · ◎ /goal active (2h)" if s["goal"] else ""))
    out.append("──────────── coordinator e2e ─\r\n")
    hint = "\x1b[2mPress up to edit queued messages\x1b[0m" if (s["queued"] and not s["typed"]) else ""
    out.append("❯\u00a0" + (s["typed"] or hint) + "\r\n")
    out.append("────────────\r\n")
    out.append("  -- INSERT -- ⏵⏵ auto mode on")
    sys.stdout.write("".join(out))
    sys.stdout.flush()


def start_turn():
    state.update(phase="turn", left=TURN_STEPS, continuing=False)


def run_queued():
    if not state["queued"]:
        return
    line = state["queued"].pop(0)
    append(SENT, line)
    if line.startswith("/compact"):
        state.update(phase="compact", left=COMPACT_STEPS)
    else:
        state["last"] = "on it: " + line[:40]
        start_turn()


def submit(line):
    line = line.strip()
    if not line:
        return
    if line in ("/goal clear", "/goal off", "/goal stop"):
        append(SENT, line)
        state["goal"] = False
        append(TRANSCRIPT, goal_entry("clear"))
        return
    if line.startswith("/goal "):
        append(SENT, line)
        state["goal"] = True
        append(TRANSCRIPT, goal_entry(line[6:]))
        if state["phase"] == "rest":
            start_turn()
        return
    state["queued"].append(line)
    if state["phase"] == "rest":
        run_queued()


def step():
    s = state
    s["elapsed"] += 1
    if s["phase"] == "compact":
        s["left"] -= 1
        if s["left"] <= 0:
            append(TRANSCRIPT, {"type": "system", "subtype": "compact_boundary",
                                "compactMetadata": {"preTokens": s["tokens"], "postTokens": 30000}})
            s.update(tokens=30000, phase="rest", elapsed=0)
            run_queued()
    elif s["phase"] == "turn":
        s["left"] -= 1
        if s["left"] <= 0:
            if s["goal"]:
                s.update(phase="hook", left=HOOK_STEPS)
            else:
                s.update(phase="rest")
                run_queued()
    elif s["phase"] == "hook":
        s["left"] -= 1
        if s["left"] <= 0:
            if s["goal"]:
                s.update(phase="cont", left=CONT_STEPS, continuing=True)
            else:
                s.update(phase="rest")
                run_queued()
    elif s["phase"] == "cont":
        s["left"] -= 1
        if s["left"] <= 0:
            start_turn() if s["goal"] else s.update(phase="rest")
    elif s["phase"] == "rest":
        run_queued()


def main():
    append(TRANSCRIPT, {"type": "x"})
    append(TRANSCRIPT, goal_entry(GOAL))
    fd = sys.stdin.fileno()
    old = termios.tcgetattr(fd)
    tty.setraw(fd)
    try:
        nxt = time.time()
        while True:
            draw()
            r, _, _ = select.select([fd], [], [], max(0, nxt - time.time()))
            if r:
                data = os.read(fd, 4096).decode("utf-8", "replace")
                for ch in data:
                    if ch in ("\r", "\n"):
                        submit(state["typed"])
                        state["typed"] = ""
                    elif ch == "\x7f":
                        state["typed"] = state["typed"][:-1]
                    elif ch == "\x03":
                        return
                    elif ch >= " ":
                        state["typed"] += ch
            if time.time() >= nxt:
                step()
                nxt = time.time() + TICK
    finally:
        termios.tcsetattr(fd, termios.TCSADRAIN, old)


if __name__ == "__main__":
    main()
