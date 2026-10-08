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

## Development

```bash
clojure -M:test     # kaocha; suites in tests.edn
```
