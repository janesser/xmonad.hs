# Switchmode smoke test — handover to small agent

You (small agent, backend cyberkleiber:8082) run the V100 **"switchmode"**
smoke test and report the results. Do NOT edit any code or model files — run
commands only and report.

## What "switchmode" means

`switch-ai-backend.sh image --batch <FILE>` swaps the single V100 from its text
backend to sd.cpp, runs the sd.cpp batch, then restores the prior text
backend. We are verifying that swap works end to end with the Flux.2-klein Q4_0
checkpoint.

## Steps

1. Record the pre-test state:
   - `nvidia-smi` — the V100 should be held by the text backend.
   - `systemctl --system is-active llama-cuda` — record whether it is active.
   - `curl -fsS http://127.0.0.1:8081/v1/models` (or :8091 for vLLM-Omni) —
     record which text backend is live (this is the backend we must restore to).
2. Note whether the two output paths below **already exist** (they should not):
   ```
   /media/sailor/ai-server/sd-probe/out_switchmode_1.png
   /media/sailor/ai-server/sd-probe/out_switchmode_2.png
   ```
3. Run the switchmode with this batch file:
   ```
   /home/jan/.local/bin/switch-ai-backend.sh \
     image --batch /home/jan/.local/share/chezmoi/discoveries/ai-server/ai-image-gen/tests/switchmode/batch-flux-klein.json \
     --deadline 900
   ```
   IMPORTANT: run it WITHOUT a leading `sudo`. The script calls `sudo systemctl`
   internally, and that is already passwordless via the sudoers drop-in; a
   leading `sudo switch-ai-backend.sh` fails with "A terminal is required to
   authenticate". Run the whole command in ONE bash call and capture its full
   output (the per-call JSON plus the restore messages) and its exit code.
4. After it finishes, for each output path verify the file is a **fresh new**
   render: it exists, is >0 bytes, and its mtime is **after** the start of this
   run (i.e. it did not exist before step 2). Quick validity check — a real PNG
   header (`ff d8 ff`) and non-trivial size; a non-black content check is a
   bonus but the required pass is "a fresh output file was generated".

## Success criteria (all must hold)

- Each call produced a **fresh new** output file (newly created by this run,
  not pre-existing).
- The generated file path(s) are reported back clearly (absolute path).
- The prior text backend was restored (llama-cuda healthy on :8081, or
  vLLM-Omni on :8091 — whatever was live before the switch).
- No orphaned llama/sd processes left holding the V100.

## Report back — write to handover-in.md

Write results to:
`/home/jan/.local/share/chezmoi/discoveries/ai-server/ai-image-gen/tests/switchmode/handover-in.md`

Fill in the template in that file: prior backend (WHO), per-call exit/seconds,
the fresh-output file path(s) with their mtime and size, whether each is a
valid PNG, whether restore succeeded, and full error output on any failure.
Also report the switchmode command's exit code.
