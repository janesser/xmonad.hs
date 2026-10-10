# sd.cpp image mode via switch-ai-backend.sh

Run image generation on the **V100 (32 GB)** by temporarily borrowing it from the
text backend, running a **batch** of sd.cpp (`stable-diffusion.cpp`) calls, then
**automatically switching back** to the text backend. This is driven by
`switch-ai-backend.sh image` — the same single script that swaps between
`instruct` (llama.cpp, :8081) and `omni` (vLLM-Omni, :8091).

> The V100 holds **exactly one** big compute backend at a time. `switch-ai-backend
>.sh image` evicts whichever text backend holds it, runs the batch, then restores
> that backend. Olla (:40114) is **not** restarted — its periodic discovery refresh
> sees whichever backend is live.

---

## When to use this (vs. `AGENT_RUN_PROMPT.md`)

- Use `switch-ai-backend.sh image` when you want a **managed, deterministic** run:
  a set of one-or-more renders handed over upfront, with an automatic switch-back
  timer and per-call result verification.
- Use the ad-hoc `gen_klein_smoke.sh` / `AGENT_RUN_PROMPT.md` delegation when you
  only need to run **one** command and report back.

---

## The command

```
switch-ai-backend.sh image --batch FILE [--deadline SECONDS]
```

- `--batch FILE` — **required**. Path to a JSON batch (see format below).
- `--deadline SECONDS` — default **300** (5 min). Hard cap for the whole batch.
  The mode switches back at whichever comes **first**: the batch finishes, or the
  deadline is hit. If a render is still running at the deadline, its sd-cli gets a
  graceful `SIGTERM` (completed PNGs are kept; the still-running call is reported
  as failed and the batch stops).

Example:

```
switch-ai-backend.sh image --batch /media/sailor/ai-server/renders/klein_batch.json --deadline 300
```

The command prints, at the end, a **JSON results array** — one object per call:

```json
[
  {"call": 1, "output": "/media/sailor/ai-server/renders/klein_a.png", "exit": 0, "seconds": 41.3, "ok": true},
  {"call": 2, "output": "/media/sailor/ai-server/renders/klein_b.png", "exit": 1, "seconds": 2.0, "ok": false}
]
```

`ok` is `true` only when exit code is 0 **and** the output file exists and is
non-empty. **Read this array to confirm what was produced** — the `output` paths
are where you find the PNGs.

---

## Batch file format (JSON)

Shared flags at the top apply to every call; each call supplies its own
`prompt`/params and **must** define `output` (the result location — the caller
owns where files land).

```json
{
  "model":  "/media/sailor/ai-server/sd-probe/diffusion_models/flux-2-klein-4b-Q4_0.gguf",
  "llm":    "/media/sailor/ai-server/sd-probe/text_encoders_gguf/flux2-klein-4b-uncensored-q8_0.gguf",
  "vae":    "/media/sailor/ai-server/sd-probe/vae/flux2-vae.safetensors",
  "vae-format": "flux2",
  "tokenizer": "/media/sailor/ai-server/sd-probe/tokenizers/flux2-klein-qwen3-tokenizer.json",
  "calls": [
    {
      "prompt": "a fox made of glass, studio lighting, shallow depth of field",
      "seed": 42, "steps": 20, "guidance": 4,
      "width": 512, "height": 512,
      "output": "/media/sailor/ai-server/renders/klein_a.png"
    },
    {
      "prompt": "a serene mountain lake at sunrise, mist over the water",
      "seed": 7, "steps": 20, "guidance": 4,
      "width": 768, "height": 768,
      "output": "/media/sailor/ai-server/renders/klein_b.png"
    }
  ]
}
```

Rules enforced by the script:

- Top-level `model` (diffusion GGUF) is **required**. `llm` / `vae` / `tokenizer`
  / `vae-format` are shared defaults for all calls — override per call if needed.
- **Each call must set `output`** — this is where its PNG goes. The script checks
  each output after the call.
- Per-call keys become sd.cpp flags (`--prompt`, `--seed`, `--steps`,
  `--guidance`, `--width`, `--height`, …). The full flag list is in
  `stable-diffusion-cpp.md`; common ones: `--steps`, `--guidance`, `--seed`,
  `--width`, `--height`, `--flow-shift`, `--negative-prompt`, `--clip`.
- A batch that hits its `--deadline` mid-run stops; a call that exits non-zero or
  produces an empty/missing output **stops the batch** (earlier outputs are kept).

---

## Where the model files live

All FLUX.2-klein-4b components are already on this box (see
`stable-diffusion-cpp.md`):

| Component | Path |
|---|---|
| diffusion GGUF (Q4_0) | `/media/sailor/ai-server/sd-probe/diffusion_models/flux-2-klein-4b-Q4_0.gguf` |
| text encoder (q8_0, Qwen3-4b) | `/media/sailor/ai-server/sd-probe/text_encoders_gguf/flux2-klein-4b-uncensored-q8_0.gguf` |
| VAE | `/media/sailor/ai-server/sd-probe/vae/flux2-vae.safetensors` (`--vae-format flux2`) |
| tokenizer | `/media/sailor/ai-server/sd-probe/tokenizers/flux2-klein-qwen3-tokenizer.json` |

Flux.2 carries three text encoders — pass them via `--clip_g`, `--t5xxl`,
`--llm` where a model needs them (not just `--llm`). The Qwen.2-klein smoke test
uses `--llm` for its single Qwen3-4b encoder.

sd-cli is at `/home/jan/projs/stable-diffusion.cpp/build/bin/sd-cli`.

---

## Agent checklist

1. **Decide the batch**: which model(s), prompts, seeds, sizes, and — critically
   — the `output` path(s). Create the destination directory first (`mkdir -p`).
2. **Estimate wall-time**: one FLUX.2-klein 512² render is ~30–60 s on the V100.
   Set `--deadline` comfortably above the total (e.g. 300 s for a handful of
   small renders). If a batch won't fit, either raise the deadline or trim it —
   a call running past the deadline is SIGTERM'd (partial work lost).
3. **Write the batch JSON** and validate it: `python3 -c 'import json;json.load(open("FILE"))'`.
4. **Run it**: `switch-ai-backend.sh image --batch FILE --deadline N`.
5. **Check results**: read the printed JSON array; confirm each `ok:true` call's
   `output` exists and is non-empty (`ls -l <png>`). Move/keep the PNGs as needed.
6. The script **auto-restores** the text backend — no manual cleanup. If the run
   was SIGTERM'd by the deadline, note which call was cut.

---

## Notes / constraints

- **NVIDIA gate**: the `image` mode refuses to run if `nvidia-smi` is absent (not
  an NVIDIA box) or if `sd-cli` is missing.
- **Does not restart Olla** (see the script header).
- **Does not umount** the shared HF cache bind mount — it must stay mounted for
  the encoders/VAE.
- `--deadline 0` disables the deadline (batch runs to completion, then switches
  back).
