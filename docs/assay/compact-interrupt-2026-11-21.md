# Assay: tmux compact interrupt, 2026-11-21

Harness: hive-assay `run/run-experiment!` (MockTransport over a real compact job),
full-factorial over `:params/arm`, 30 paired replicates (same seed -> same pane
timeline in every arm). Pane: `test/hive_tmux/claude/goal_pane.clj`, a fake
Claude Code /goal pane in one endless turn (100 s `ss wait`, thinking gaps,
`sleep`, `ship land` writes of 1-10 min; p=0.25 one turn boundary in 3 h).
Key reactions measured on Claude Code 2.1.330 (queued /compact, C-x C-s
backgrounds the tool and sends now; Esc stops the turn).
Oracle: compacted AND goal restored AND no write cut. Time = fake clock from
job start to /compact sent. Defaults: interrupt_after_ms 300000, poll 5 s,
idle timeout 30 min. Reproduce: `dev/assay_compact_interrupt.clj`.

| write share | arm | pass | rate | Wilson 95% | writes cut | ttc median / p90 / max (s) |
|---|---|---|---|---|---|---|
| 0.15 | old-job (interrupt=false) | 0/30 | 0.00 | [0.00, 0.11] | 0 | n/a (all :timeout :wait-idle) |
| 0.15 | interrupt=auto, force-send | 30/30 | 1.00 | [0.89, 1.00] | 0 | 300 / 670 / 860 |
| 0.15 | interrupt=auto, escape | 30/30 | 1.00 | [0.89, 1.00] | 0 | 306 / 676 / 866 |
| 0.50 | old-job | 0/30 | 0.00 | [0.00, 0.11] | 0 | n/a |
| 0.50 | interrupt=auto, force-send | 27/30 | 0.90 | [0.74, 0.97] | 0 | 570 / 1125 / 1605 |
| 0.50 | interrupt=auto, escape | 27/30 | 0.90 | [0.74, 0.97] | 0 | 576 / 1131 / 1611 |

The 3 failures at write share 0.5 are :timeout panes where writes covered every
read of the 30 min window: the gate held, as designed. Force-send and escape tie
on the fake pane. They differ in reality only in what happens to the running
tool (backgrounded vs killed), which the fake pane cannot price. That argues
for force-send as the default.
