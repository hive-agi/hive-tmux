# hive-tmux

A tmux terminal backend for [hive-mcp](https://github.com/hive-agi/hive-mcp).
It implements the terminal (`ITerminalAddon`) and vessel (`IVessel`) ports, so
lings can run in tmux panes, and drives tmux through the Python
[libtmux](https://libtmux.git-pull.com/) library via
[libpython-clj](https://github.com/clj-python/libpython-clj).

License: MIT.

## Coordinate

```clojure
io.github.hive-agi/hive-tmux {:mvn/version "<version>"}
```

Published to Clojars. The current version is in `VERSION`.

## Prerequisites

The addon needs these at runtime, and none of them comes with the jar:

- **tmux** on `PATH`.
- **Python 3**, the interpreter that libpython-clj binds.
- **libtmux** installed for *that* interpreter (`python -m pip install libtmux`).

Pick the interpreter with `HIVE_PYTHON_EXECUTABLE` (or `HIVE_TMUX_PYTHON`),
pointing at a Python that has libtmux (for example a conda env). If neither is
set, a conda env under `$HOME` is auto-detected; failing that, libpython-clj
autodetects one.

If a prerequisite is missing, the vessel stays **dormant**: the addon mounts,
but it reports through its preflight / health status instead of opening panes.
That looks the same as "not installed", so check those three items before you
treat it as a mount bug.

## Mounting

You do not need to wire anything. The jar ships the manifest
`META-INF/hive-addons/hive-tmux.edn` (`:addon/id "hive.tmux"`), and
hive-mcp's classpath manifest scanner mounts it automatically.

- `:addon/init-ns` is `hive-tmux.init`.
- `:addon/init-fn` is `addon-ctor`, a constructor that returns an `IAddon`.
  The host registers and initializes the addon it returns.
- `init-as-addon!` is retained for the legacy loader, which expects the addon to
  register itself.

Capabilities: `:terminal`, `:health-reporting`, `:vessel`.

## Headed sessions

The `swarm` tool discovers Claude Code and Codex terminals through tmux:

```json
{"command":"tmux panes","pane_provider":"codex"}
{"command":"tmux read","pane":"%3","lines":20}
```

`pane_provider` defaults to `all`; `claude` and `codex` select one provider.
Each row reports its provider and capabilities. Codex rows expose terminal
identity and the `read-screen` capability. Task identity, busy/input state,
context usage and goal state remain unknown. Titles are display metadata.

Codex screen reads return terminal text with ANSI escapes removed. Claude
screen reads can additionally return parsed state and transcript text.
Prompt submission, goal control and compaction currently require Claude Code;
automatic compaction sweeps cover Claude panes. A mixed explicit compaction
selection containing Codex starts no jobs.

These inspection commands use the tmux CLI and work independently of the
Python bridge used to create managed terminal vessels.

## tmux event ingestion (observe socket)

The addon listens on a Unix socket for events sent by tmux hooks. The
default path follows the hive socket standard (STD-04 T2):

- `$XDG_RUNTIME_DIR/hive/tmux-observe.sock` when `XDG_RUNTIME_DIR` is set,
- else `<java.io.tmpdir>/hive-<uid>/tmux-observe.sock`.

The directory is created 0700 and the socket is 0600. If a live listener
already answers on the path, a second instance does not start and does not
touch tmux hooks. `:tmux/observe-socket` sets an explicit path.

Hooks are installed only with `:tmux/observe-install-hooks? true`. An
installed hook always carries the listener's resolved path. The hook
client (`hive-tmux-hook <socket> <hook> <pane>`) also accepts `-` as the
socket, meaning the default path above. With `XDG_RUNTIME_DIR` unset, `-`
assumes `/tmp`, so it does not match a JVM started with
`-Djava.io.tmpdir=<dir>`. Pass the explicit path in that case.

### Upgrading from a release before the STD-04 socket path

The old default was `$XDG_RUNTIME_DIR/hive-tmux-observe.sock` (or
`/tmp/hive-tmux-observe.sock`). Hooks installed by an older release keep
that path in their `run-shell` command. After the upgrade the hook client
exits 1 on every firing and the events are lost with no log line in hive.
Reinstall the hooks once after upgrading:

- with `:tmux/observe-install-hooks? true`: restart the addon (or hive-mcp);
  its start reinstalls every hook with the new path;
- with hooks you installed by hand: rerun your `set-hook -g` commands with
  the new path, or set `:tmux/observe-socket` to the old path.

Check with `tmux show-hooks -g`: every hive-tmux hook must name the new
socket path.

## Development

```bash
clojure -M:test     # kaocha; suites in tests.edn
```
