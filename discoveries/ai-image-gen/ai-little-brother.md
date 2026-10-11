# Handover to the little brother (small agent) — switchmode smoke test

Status: **INCOMPLETE** (2026-10-11) — the delegation run was aborted before a
clean pass. The harness and batch are written and validated; the V100
"switchmode" swap has **not** been executed end-to-end via the small agent yet.

## What this is

The "little brother" = the small-context agent that runs on the cyberkleiber
Iris-Xe backend (`:8082`), leaving the single V100 free. It runs the **switchmode**:
swap the V100 from its text backend to sd.cpp, run an image batch, then restore
the text backend. This file records that handover and the current (incomplete)
discovery state.

## The harness / batch files

- batch spec: `tests/switchmode/batch-flux-klein.json`
  (Flux.2-klein Q4_0, 2 calls — fox-of-glass seed 42, abstract neon landscape
  seed 7; outputs to `/media/sailor/ai-server/sd-probe/out_switchmode_{1,2}.png`).
- prompt to the little brother: `tests/switchmode/handover-out.md`
- results template: `tests/switchmode/handover-in.md`

## How to run (for the little brother)

Follow `tests/switchmode/handover-out.md` — it steps through: pre-state health
(the V100 held by the text backend, `llama-cuda` active on :8081), the
switchmode itself, the two fresh-output PNGs, and the restore.

### CRITICAL gotcha — run the script bare, no leading `sudo`

`/home/jan/.local/bin/switch-ai-backend.sh` does its **own** passwordless
`sudo systemctl` internally (covered by the `chezmoi-pi` sudoers drop-in). Run
it like this:

    /home/jan/.local/bin/switch-ai-backend.sh \
      image --batch tests/switchmode/batch-flux-klein.json --deadline 900

A leading `sudo switch-ai-backend.sh …` fails with
*"A terminal is required to authenticate"* and, on a non-interactive shell,
**hangs forever on the password prompt**. That prompt is what killed the
2026-10-11 run (the small agent stalled for 25 min until the wrapper's timeout
aborted it).

### Other notes

- The V100 text backend (:8081, `llama-cuda`) goes offline during the render and
  is restored automatically at the end of the batch. This big session rides the
  same backend, so it is idle during the run and resumes after.
- Keep a short timeout on the run so a hang fails fast (the 25-min timeout cost
  a full session).
- **Pass bar:** each call generates a **fresh** (newly-created, >0 byte) output
  PNG and its absolute path is reported back; the prior text backend is
  restored; no orphaned processes left holding the V100.

## Why it is incomplete

- 2026-10-11: first delegation attempt hung on the leading-sudo password prompt
  (see gotcha above); aborted via the wrapper `timeout`.
- Harness + batch validated beforehand: `batch-flux-klein.json` parses, all four
  model components present (diffusion 2.46 GiB, text encoder 4 GiB, VAE,
  tokenizer), both output paths pre-checked absent.
- `handover-out.md` was then corrected to drop the leading `sudo`.

## Next

Re-run via the little brother using the bare command (no leading sudo) with a
tighter timeout. Once it produces two fresh output PNGs and restores :8081, the
switchmode is verified — then mark this discovery's switchmode item complete.
