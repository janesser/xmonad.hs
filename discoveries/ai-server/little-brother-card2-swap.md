# Little Brother on Card 2 — llama.cpp / sd.cpp swap system (Era 1)

**Status: active (2026-10-23)** — design complete, not yet implemented. Next step:
implement units + swap script under chezmoi, then smoke-test. Supersedes the
switchmode eviction dance in `ai-little-brother.md` (that doc's protocol exists
to keep the *only* text backend alive while the V100 is borrowed; with a
dedicated card-2 text backend, the problem disappears).

**Forge provenance:** `_bmad-output/forge/vllm-dual-gpu-migration/.memlog.md`.

## 1. Goal

Card 2 (currently the V100-PCIe-32GB, `GPU-fb929bd0-1296-491c-ed72-8f8494addcb8`,
sitting idle at ~11 MiB) hosts, **without ever touching the 27B on CUDA0**:

- the **little brother** — small auxiliary model, text backend, 24/7
  (sub-agent work, cheap/fast tasks, image-batch operator);
- **sd.cpp** (Flux.2-klein Q4_0, ~6.5 GiB: diffusion 2.46 + text encoder 4 +
  VAE + tokenizer) — on demand, for image batches.

## 2. Architecture

```
CUDA0 (V100 SXM2, 32GB)          CUDA1 (V100 PCIe, 32GB)
+----------------------------+   +----------------------------+
| llama serve (router)       |   | llama-little-brother      |
| Qwen3.8-27B Q4_K_M         |   | (resident, TEXT mode)     |
| --parallel 4, cache-ram    |   | [::]:8082  <- old SYCL    |
| 12288, 4 agent sessions    |   | port/contract, now CUDA   |
| :8081                      |   +----------------------------+
+----------------------------+   | (IMAGE mode, transient)   |
                                  | sd.cpp batch under        |
                                  | timeout, then back        |
                                  +----------------------------+
```

- **TEXT mode (default, resident):** little-brother llama.cpp server, always
  up. This is the permanent "a text backend keeps answering" guarantee that
  the old mutual-support handover protocol used to manufacture manually.
- **IMAGE mode (transient):** the swap script stops the little-brother unit,
  runs the sd.cpp batch (GPU pinned, deadline-enforced), then restarts the
  unit. One transient systemd unit owns the whole three-phase sequence, so a
  dead caller cannot strand the card.
- **`--keep-text` escape hatch:** run sd.cpp *alongside* the little brother
  without stopping it (fits: ~2 GB + 6.5 GB ≪ 32 GB). Text gets slower during
  the batch. Default is the exclusive swap; the hatch exists for when the
  little brother must not blip.

## 3. Units and files (chezmoi-tracked)

| File | Lands at | Role |
|---|---|---|
| `etc/systemd/system/llama-little-brother.service` | `/etc/systemd/system/` | resident text backend, enabled, `Restart=on-failure` |
| `dot_local/bin/executable_start-little-brother.sh` | `~/.local/bin/` | launcher: reads model file, execs `llama serve` |
| `dot_local/bin/executable_card2-swap.sh` | `~/.local/bin/` | the swap system (below) |
| `dot_config/card2/little-brother.model` | `~/.config/card2/` | one line: HF repo id or local GGUF path (the "varying second model") |

**Pinning:** the unit sets `Environment=CUDA_VISIBLE_DEVICES=<card-2 UUID>`
and the server runs `-dev CUDA0` (index 0 in the remapped space). Pin by
UUID, never by global index — survives reordering; re-pin once when the card
is physically swapped (Era 2). Port **8082** is reused from the dead SYCL
backend, so anything already pointed at :8082 (pi handover config, little
brother agent) keeps its contract.

**Unit skeleton:**

```ini
[Unit]
Description=Little brother — llama.cpp text backend on card 2 (PCIe V100), [::]:8082
After=local-fs.target

[Service]
Type=exec
Environment=CUDA_VISIBLE_DEVICES=GPU-fb929bd0-1296-491c-ed72-8f8494addcb8
ExecStart=/home/jan/.local/bin/start-little-brother.sh
Restart=on-failure
RestartSec=5
```

`start-little-brother.sh` (mirrors the existing `restart-llama-*.sh` launcher
pattern): read `~/.config/card2/little-brother.model` (default
`unsloth/Qwen3.5-0.8B-GGUF:Q4_K_M` — what the SYCL unit last carried), then
`exec llama serve --host :: --port 8082 -dev CUDA0 --no-webui --metrics
--parallel 2 --hf-repo "$MODEL" ...`. Changing the model = edit the one-line
file + `systemctl restart llama-little-brother`.

**sd.cpp gets NO persistent unit.** It is a batch tool, not a daemon: the
swap script runs it as a child under `timeout <deadline>` inside a transient
unit (below). A resident sd.cpp service would burn ~20 W and 6.5 GiB for
minutes-of-use-per-week.

## 4. The swap script — `card2-swap.sh`

```
card2-swap.sh image --batch <spec.json> [--deadline 900] [--detach] [--keep-text]
card2-swap.sh text                  # force return to TEXT mode
card2-swap.sh status                # mode, since, current unit, last result
```

- **Single-flight:** `flock` on `~/.local/share/card2/lock` (auto-released on
  process death — no stale-lock recovery needed) + state file
  `~/.local/share/card2/state` (`mode|timestamp|unit|result`).
- **`image`:** writes state=`swapping`; `systemctl stop llama-little-brother`;
  runs the existing sd.cpp batch runner (the switchmode harness in
  `ai-image-gen/tests/switchmode/` — batch spec format unchanged, GPU pinned
  to the card-2 UUID) under `timeout <deadline>`; **`trap EXIT` restarts the
  little-brother unit no matter how the batch exits** (success, failure,
  timeout, signal); writes state=`text` + last result; on text-restart
  failure: state=`degraded` + loud journal line (the card is up, the text
  backend is not — that is the one state a human must see).
- **`--detach` (the self-swap path):** instead of running in the caller's
  process tree, the script registers itself as a transient root unit —
  `systemd-run --unit=card2-image-<timestamp> bash -c '<same sequence>'` —
  and returns immediately with the unit name. The whole three-phase sequence
  is then owned by systemd, not by any agent or shell.
- **Why transient unit, not a persistent one:** batch params (spec, deadline)
  are per-invocation; the unit must live exactly as long as the swap; and
  `systemctl` visibility (`is-active card2-image-*`) is the status surface.

## 5. The self-swap subtlety (little brother operating its own card)

The little brother's model *is* the process the swap kills. So the agent
cannot run the swap synchronously in its own turn — it would die mid-thought
and the batch would be orphaned. Contract:

1. Agent calls `card2-swap.sh image --batch … --detach` → gets unit name.
2. Agent **ends its turn** (its backend is about to go away — nothing to do
   but stop; the transient unit survives independently).
3. Completion is observed out-of-band: `systemctl is-active card2-image-*`,
   the state file's result, or the batch's output PNGs (the existing pass
   bar: fresh >0-byte PNGs + reported paths).
4. On the agent's next wake, text mode is restored (or `degraded` — then page
   a human).

Humans can of course run the same command without `--detach` from a shell.

## 6. What dies with this (retirements, in the same change)

- `llama-sycl.service` — zombie crash-loop (214+ restarts, no Intel device on
  this box): stop, disable, remove unit; the oneAPI bootstrap path dies with
  it.
- **Mutual-support handover protocol** — `llama-handover-request.sh`,
  `llama-handover.sh`, marker + event log under
  `~/.local/share/llama-cpp/handover/`, and the AGENTS.md section: obsolete.
  Stopping `llama-cuda` (CUDA0) no longer threatens "no text backend at all"
  — :8082 is permanently up.
- `switch-ai-backend.sh` (CUDA0 text↔sd.cpp eviction swap): obsolete for
  normal operation — sd.cpp no longer borrows the 27B's card. Keep the
  recipes documented here as an escape hatch only if a future image model
  outgrows the PCIe card.
- Olla is already inactive; no action. The main router on :8081 is untouched.

## 7. Failure modes and their handling

| Failure | Handling |
|---|---|
| Batch exceeds deadline | `timeout` kills sd.cpp; trap restores TEXT |
| sd.cpp crashes mid-batch | trap restores TEXT; state=failed + journal |
| Little-brother restart fails after batch | state=`degraded`, journal alert; card free; manual `card2-swap.sh text` or `systemctl restart` |
| Caller (agent/shell) dies mid-swap | transient unit continues; single-flight lock held by the unit's process |
| Two batches at once | second `flock` wait fails fast → "already swapping" |
| Card 2 dies / is removed | little-brother unit fails (visible); card-2 services masked; little brother falls back to co-residence on CUDA0 (config change only — 27B Q4 + 0.8B + 4×32k KV ≈ 28 GiB fits) |

## 8. Verification (pass bar)

1. `card2-swap.sh status` → `text`; little brother answers on :8082.
2. `card2-swap.sh image --batch <spec> --deadline 900` (foreground, human
   shell) → fresh PNGs produced, state back to `text`, little brother answers
   again, `journalctl -u llama-little-brother` shows clean stop/start pair.
3. `--detach` path: trigger from a little-brother turn, unit completes,
   agent resumes on restored backend.
4. Kill -9 the batch mid-run → trap still restores TEXT.
5. `systemctl disable --now llama-sycl` verified gone; no handover marker
   written on a `llama-cuda` stop.

## 9. Era-2 note (confirmed 2026-10-23)

Confirmed by Jan: the PCIe V100 stays for the whole of Era 1 and is
*replaced by a second SXM2* in Era 2. So this design is the permanent
card-2 architecture, not a bridge: at swap time re-pin the UUID in
`llama-little-brother.service` (one line), everything else survives, and
card 2 gains NVLink for free (see `../devices/hp_z6_g4/dual-gpu.md`.
