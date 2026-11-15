<!-- bmad:context -->
<!-- Verified 2026-11-06 on `flowerpower` by path-checking disk state; standalone target — no git SHA to diff from. Managed by bmad-project-context; edits inside this block are replaced on refresh. Keep anything you want preserved outside the markers. -->

## Orientation

This is jan's global agent instruction file: the instructions pi-agent loads in every working directory, unlike `~/.local/share/chezmoi/AGENTS.md`, which only loads inside the chezmoi repo. It is a single chezmoi template rendered on each of five fleet members — so every claim below carries its host, and you must run `hostname` before applying any of them. Repo-internal deploy details live in the repo's own AGENTS.md, not here.

## Host identity (apply before this file)

Determine your fleet member first: `hostname` → `flowerpower`, `raspberrypi`, `cyberkleiber`, `lincopta`, or `peacewagon`. Views below are qualified per machine; a claim with no host tag applies to all. Reachability splits LAN (no hostname suffix) from WAN via fritzbox DDNS (`*.wan`).
- `flowerpower`: home desktop, agent config rendered here.
- `raspberrypi`: router/NAS; proxied SSH to `cyberkleiber` via `.lan`, WAN through `fritz.net`.
- `cyberkleiber`: reachable only via `ProxyJump raspberrypi.wan`; herdr remote host.
- `lincopta`: direct SSH on LAN, age keys shared.
- `peacewagon`: age key recipient only; not in SSH config.

## Policy

- This file is rendered by chezmoi from `dot_pi/agent/AGENTS.md` — edit the source, never the render; every machine gets a different copy of the same template.
- Privileged steps for `cz update` must run through `/etc/sudoers.d/base`; do not sudo outside a chezmoi run script; if a needed command is missing, stop and ask.
- Never edit `/etc/sudoers.d/base`.
- Workflows must run through BMAD skills where they exist (see Skill direction) — don't reinvent them in-chat.
- Editing this file: change `~/.local/share/chezmoi/dot_pi/agent/AGENTS.md` first, then `cz re-add` or `cz add`, then `cz apply` to verify — never edit the render directly.

## Where things are

(flowerpower) Agent config: `~/.pi/agent/` — `settings.json`, `skills/`, `sessions/`. Persistent memory in `pi-hermes-memory/`.
(flowerpower) Dotfiles source: `~/.local/share/chezmoi/` — the repo (git remote `janesser/xmonad.hs.git`); `cz apply` installs, `cz update` pulls and applies.
All machines: skills load path `~/.agents/skills/` — symlinks to `/usr/share/omarchy/default/agents/skills/`; bmad skills are published there by the agentic script (bmad's own default placement, no staging step), the legacy `~/.pi/agent/skills/bmad-*` copies were removed.

## Image generation (Flux.3, V100)

When asked to generate or redesign logos/markers/images, **don't hand-write SVG** — use the local Flux.3 diffusion model on the shared V100 via `switch-ai-backend.sh`:

```bash
# 1. write a JSON batch file (one entry per variant; must define 'output')
# 2. run it: the script stops llama.cpp, runs sd.cpp, then restores it
/home/jan/.local/bin/switch-ai-backend.sh image --batch /tmp/batch.json --deadline 900
```

Batch template — shared top-level flags + a `calls` array; each call sets its own prompt/params:

```json
{
  "model": "/media/sailor/ai-server/sd-probe/diffusion_models_gguf/flux3-image-klein-base-Q8_0.gguf",
  "llm": "/media/sailor/ai-server/sd-probe/text_encoders_gguf/Qwen3VL-4B-Instruct-Q8_0.gguf",
  "vae": "/media/sailor/ai-server/sd-probe/vae/flux2-vae.safetensors",
  "vae-format": "flux2",
  "tokenizer": "/media/sailor/ai-server/sd-probe/tokenizers/flux2-klein-qwen3-tokenizer.json",
  "guidance": 7.5,
  "steps": 30,
  "width": 1024,
  "height": 1024,
  "flow-shift": 1.0,
  "calls": [
    {"prompt": "...", "seed": 1, "output": "/tmp/out/variant_1.png"}
  ]
}
```

- The script switches the V100 to sd.cpp mode (default deadline 300s; `--deadline 900` for longer batches), runs each call in sequence, then restores llama.cpp on :8081.
- Each 1024×1024 call took ~45–65s. Batch of 5 → ~5 min.
- Outputs land wherever `output` points; copy them to the target host afterwards via `scp`.
- Verify with `nvidia-smi` before/after: a stalled batch can hold the card and starve llama.cpp on restore.

## Running and verifying

- A claim is only load-bearing if you can verify it **and** its removal changes agent behavior: assert things are real, assert things have impact. Unverified claims are candidates for removal; unimpacted claims are noise to cut.
- Every claim below is unverified until you path-check it for this machine; if you cannot, treat it as false and ask before acting.
- Verify every command or unit claim by running it — never by reading the block that describes it: `systemctl --system is-enabled <unit>` on this box, SSH into cyberkleiber or raspberrypi otherwise.
- bmad skills are published by the agentic script's one-liner (`npx --yes bmad-method install --yes --tools pi,opencode` — where they land is bmad's decision) once pi-agent is settled; they land in `~/.agents/skills/`, which both tools read natively. Flush-and-fill means nothing needs restoring after an install, so no trigger scripts remain. Project-local `_bmad/` installs are obsolete. One publication point: pi-agent (Agent Skills location) and OpenCode (global compatibility path). No tool-specific config entry needed for either.
- New files: create at the target location first, then `chezmoi add <target>` to register — registration moves it into the source tree and makes it managed. Never create in the source tree and rely on `cz apply`; a file registered only in git is not managed by chezmoi.

## Skill direction

- AGENTS.md changes: `bmad-project-context` (refresh to re-verify, audit to assert claims) — never hand-edit the block.
- Research to support a decision: `bmad-deep-recon` (run here, or draft prompt for your own tool).
- Everything else that has a BMAD skill: run the skill, don't write prose instructions around it.

## Known pitfalls

- Systemd units are per-machine and stale by default — `restart-llama-server.service` does not exist (`systemctl --system is-enabled` → not-found) on this box; llama.cpp starts from a chezmoi script instead.
- `auto-poweroff.*` timer and its inputs exist as chezmoi source (`.chezmoiscripts/run_onchange_8_auto_poweroff_timer.sh.tmpl` with `etc/systemd/system/auto-poweroff.times`, `.delay`) but are not deployed to `/etc/systemd/system/`; the mechanism matters, the values change.
- Fish overrides `SSH_AUTH_SOCK` when herder's proxy socket is dead — the shell sees the live system agent, not herder's.

<!-- /bmad:context -->
