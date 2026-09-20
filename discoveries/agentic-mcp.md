# Agentic MCP servers via ToolHive + todo-task (NVIDIA)

Status: **documented, script removed.** The command sequence below was lifted
from `.chezmoiscripts/run_once_5_aitools_2mcp.sh` (marked `# UNUSED` in-file).
It is recorded here as handover documentation; the source script has been
removed.

Related: other AI backend setup in `.chezmoiscripts/run_once_5_aitools_*`.

---

## TL;DR

On an NVIDIA box, install the `todo-task` MCP server CLI via snap, clone
[Stacklok ToolHive](https://github.com/stacklok/toolhive) (an MCP server hub /
orchestrator), then run `task install` to bring the MCP stack online.

The whole thing is gated behind an NVIDIA-presence check so it never runs (or
installs anything) on a non-NVIDIA machine.

---

## 1. NVIDIA presence guard

Uses `lspci` to detect the GPU:

```bash
if ! lspci 2>/dev/null | grep -iq nvidia; then
    echo "$(basename $0): No NVIDIA GPU detected, skipping..."
    exit 0
fi
```

`lspci` needs no sudo and detects the GPU even with no driver loaded — the
right presence check here (`lsmod` would false-negative if the module isn't
loaded yet, e.g. on a fresh setup).

## 2. Install the `todo-task` CLI

```bash
sudo snap install task --classic
```

`task` is the `todo-task` MCP server CLI (GoToDo-OS/todo-task). `--classic`
grants the broadened confinement snap needs to talk to MCP tooling.

## 3. Clone ToolHive and install

```bash
cd ~/projs
git clone https://github.com/stacklok/toolhive.git
cd toolhive
task install
```

ToolHive is Stacklok's MCP server hub; `task install` wires the local MCP
servers (including `todo-task`) into it.

---

## Why it was removed

Marked `# UNUSED` in-file and flagged as dead code in
`chezmoiscripts.dep.yml`. Not wired into the live pi-agent package list
(`~/.pi/agent/settings.json` `packages`), so it never actually provisioned an
MCP server for the agent. Kept here as documentation in case the ToolHive +
`todo-task` approach is revisited.

## Re-enabling (if desired)

Recreate `.chezmoiscripts/run_once_5_aitools_2mcp.sh` from the commands above
(add it back to `chezmoiscripts.dep.yml`), or fold the steps into an existing
AI-tools run script once the MCP wiring is solid.
