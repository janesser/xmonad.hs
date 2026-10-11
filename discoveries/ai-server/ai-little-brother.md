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

## Mutual-support handover (llama-cuda → llama-sycl)

Stopping `llama-cuda` (:8081, V100) triggers the mutual-support handover: the big
session's `pi` backend is moved to `llama-sycl` (:8082, Iris Xe) so a text
backend keeps answering. This is exactly what the switchmode does when it drops
the V100's text backend to hand the card to sd.cpp — and it is the handover used
whenever `llama-cuda` is stopped for any reason on cyberkleiber. (The two backends
are boot units, `WantedBy=multi-user.target`, so they auto-start on wake; this
handover is the only manual case.)

**Only step 1 is a `pi-command`, and only step 1 names an explicit `--provider`
(:8082). `defaultProvider` is never used or changed at any point.** Flow:

1. **Record the stop (mandatory trigger).** Before stopping `llama-cuda`, run
   `~/.local/bin/llama-handover-request.sh`. It auto-picks the handover model
   (first id from `curl :8082/v1/models`), embeds the ready `pi --provider …
   --model …` as `pi_command` in the marker, and **triggers the executor (step 2)**:

   ```bash
   ~/.local/bin/llama-handover-request.sh "stop" "systemctl stop llama-cuda"
   ```

2. **Execute (executor — a result of step 1's call).** Stops `llama-cuda` (:8081),
   confirms :8082 answers, and **leaves the marker** in place (it holds the ready
   `pi_command`). Runs no `pi --provider`.
3. **Caller resumes (human-triggered).** Reads `pi_command` from the marker at the
   predefined location `~/.local/share/llama-cpp/handover/requested` and runs it —
   the sole `pi --provider` in the flow:

   ```bash
   eval "$(python3 -c 'import json;print(json.load(open("$HOME/.local/share/llama-cpp/handover/requested"))["pi_command"])')"
   ```

4. **Return.** Restart `llama-cuda`, clear the marker. No `--provider`, never
   `defaultProvider`:

   ```bash
   sudo systemctl restart llama-cuda          # :8081 back up
   MODEL=$(curl -fsS http://127.0.0.1:8081/v1/models \
     | python3 -c 'import json,sys;print(json.load(sys.stdin)["data"][0]["id"])')
   pi --model "$MODEL"
   rm -f "$HOME/.local/share/llama-cpp/handover/requested"
   ```

Marker + append-only event log live under
`~/.local/share/llama-cpp/handover/` (stamped with `PI_SESSION_ID`). Full spec:
`AGENTS.md` "Mutual support protocol" section. Helper sources:
`dot_local/bin/executable_llama-handover-request.sh` and
`dot_local/bin/executable_llama-handover.sh`
(both reinstalled to `~/.local/bin/`).

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
