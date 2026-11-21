# Design note: `tmux compact interrupt=true`

> **Status, 2026-11-21: implemented** as `interrupt=auto|true|false` on
> `fix/compact-interrupt` (`hive-tmux.claude.interrupt`). The consent gate
> below was replaced by a screen gate: auto cuts in only on a /goal pane after
> `interrupt_after_ms` while a read-only wait or no tool runs, never mid-write
> or over a draft. The default `interrupt_via=force-send` (queued /compact +
> C-x C-s) backgrounds the running tool instead of killing it. Measured:
> docs/assay/compact-interrupt-2026-11-21.md. The text below is the original
> note, kept for its reasoning.

Card 20261120021807-1a6075cf, follow-up of 2026-11-20 02:35. Not implemented
in feat/tmux-job-cancel.

## Problem

`tmux compact` waits for rest before it types: a settled idle, or on a
`/goal` pane the single turn boundary between two turns. A pane whose one
`/goal` turn never ends gives it neither. %5 ran one turn for 2h15m, up to
509k tokens. The job then times out after `:idle-timeout-ms` (30 min) and the
context keeps growing. Cancel and supersede (this branch) free the slot, but
they do not make a boundary.

## Shape (opt-in)

`swarm tmux compact pane=<ref> interrupt=true`, explicit pane only (never in
a sweep, the same rule as `supersede`).

1. Wait for rest as today, up to a short `interrupt_after_ms`.
2. Still working: send ONE interrupt as the composer state requires: Esc
   twice in vim NORMAL mode, once in INSERT mode (`:vim-mode` on the row).
   Record it as the step `:interrupt`. It is a keystroke, so it is claimed
   through `claim-typing` and the job can no longer be cancelled.
3. Take the turn boundary that follows (`poll/wait-turn-boundary`), then run
   the normal compact, re-seed and goal restore.
4. No boundary within `:start-timeout-ms` of the interrupt: fail `:timeout
   :during :interrupt`. Never send a second interrupt.

## Consent gate

An interrupt cuts a running tool call: a half-written file, a commit in
flight, a migration. So it is refused unless consent is evidenced:

- the pane's coordinator has posted a WRAPPED hand-off since its turn began
  (the hive-mind log is the evidence); or
- the operator grants it for that pane through the guard's permission flow.

A refusal names the gate and how to satisfy it. The decision is a pure
function over the row, the hand-off evidence and the grant. Only the boundary
reads the hive-mind and sends the key.

## Why out of scope here

This branch only takes slots and never types more than before: cancel and
supersede act only on jobs that have not typed. Interrupting is a new kind of
keystroke into a WORKING turn, which every job here is built never to do. It
needs its own consent model, a live-pane verification plan and review, and it
touches `poll` and the composer. That is not small, so it belongs on its own
card.

## Queued human messages (2026-11-21, job cc-a49d0119)

Measured on Claude Code 2.1.330: a message submitted while a turn runs is
drawn above the spinner as `❯ text` (background 237) over
`⎿  Queued · ctrl+x ctrl+s to send now`, and the empty composer shows
`Press up to edit queued messages`. The message was SUBMITTED: it is not a
draft. `screen/read-screen` reports it under `:queued`, never `:input`.

The force-send interrupt sends the queue first with C-x C-s (step
`:human-sent`, job key `:human-sent`), so the human's intent is delivered,
never dropped, and then types `/compact`. A `/compact` already in the queue
goes out with that flush and is not typed twice. With a message queued, an
`interrupt_via=escape` job force-sends instead: Esc would cut the turn that
takes the message in. An unsent draft at the prompt still holds the job
(`:prompt-holds-text`).

## The goal without a /goal entry

A goal can be set without a `<command-name>/goal` transcript entry (session
15c7760e). Claude Code still writes `{"type":"goal_status","condition":...}`
each time it checks the goal, so `panes/goal-text` reads the newest entry of
either kind (a met goal is none). With neither, the job records
`:goal-unreadable` (step and job key): the goal will not be restored, and the
pane is compacted anyway.

Assay, queued-human scenario, n=20 per arm: force-send 20/20, escape 20/20,
old job 0/20, 0 writes cut.
