# Project registry

Single root for project identity (PRD FR-5/FR-6). One `## <name>` section per
project, with `- key: value` lines:

- `remote:` — the canonical identity (the git remote). The client normalizes
  it to a project key (`https://host/path`) before any hub call (AD-10).
- `local:` — advisory checkout path(s) on this box; may be absent — project
  status must stay resolvable without a local checkout. Multiple lines allowed.
- `docs:` — discovery documents in this repo (human-facing links).

The file is read directly from the chezmoi repo by the pi-hermes-memory
client (story 2, decision Q2=d) — `cz update` keeps it current on every box.
The registry FORMAT and the parser in `pi-hermes-memory/src/hub-client/registry.ts`
change in the same change (spine seam). Unknown `- key:` lines are ignored by
readers, so the format can evolve additively.

## chezmoi
- remote: git@github.com:janesser/xmonad.hs.git
- local: ~/.local/share/chezmoi
- docs: chezmoi-evolution-sources.md

## pi-hermes-memory
- remote: git@github.com:janesser/pi-hermes-memory.git
- local: ~/projs/pi-hermes-memory
- docs: ai-graph-knowledge.md

## btop
- remote: git@github.com:janesser/btop.git
- local: ~/projs/btop-intel-gpu
- local: ~/projs/btop

## crossbar
- remote: git@github.com:janesser/Crossbar.git
- local: ~/projs/crossbar
