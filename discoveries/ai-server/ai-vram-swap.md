# Strata — run a huge MoE on a consumer PC

> **Status: held (2026-10-11)** — added to discoveries, no work yet.

* Repo: <https://github.com/Niko1221/Strata>
* What: runs **Qwen3.8-Flash-Next** (a ~125B-parameter Mixture-of-Experts
  model) on an ordinary gaming PC (12 GB+ VRAM, NVIDIA or AMD, Win/Linux).
* License: MIT (open source).

## Why it's interesting (VRAM-swap angle)

Strata shaves a model that normally needs hundreds of GB of GPU memory down to
a consumer box by **spreading the work across the whole machine**:

- The model is a team of **24,576 small "experts"**; each token only needs ~10.
- The **GPU** holds the few thousand most-used experts; **RAM** holds all of
  them; the **CPU** works the rest in parallel; a big **lookup table** lives on
  the **SSD**.
- A small helper **guesses** the next words, the big model **checks** them
  all at once → 1.6–1.8× sooner (speculative-style, not speculative decoding).
- This is the "counter/pantry" model — hot stuff on the GPU, cold on RAM/SSD.
  Relevant to the V100 <-> Iris Xe VRAM-swap work (switchmode).

## Perf (rough, from the README)

- NVIDIA RTX 5070 (12 GB): Q2_0 ~94 tok/s write, up to ~2,650 tok/s prompt read.
- AMD RX 9070 XT (16 GB): Q2_0 ~60 tok/s write.
- More VRAM = faster; an RTX 3090 (24 GB) should write ~100–140 tok/s.
- SSD-backed sizes (UD-IQ4_XS / UD-Q4_K_XL) are slow when free RAM < ~80 GB
  (reads from SSD while answering).

## Using it

- Install: `./setup.sh` (Linux) or `START-HERE.bat` (Windows). Downloads ~70 GB
  model, prompts for model/size/context, pictures on/off.
- Serves an **OpenAI-compatible** endpoint at `http://127.0.0.1:8080`
  (base URL `http://127.0.0.1:8080/v1` for any OpenAI-API app). Anthropic apps
  use `/v1/messages`; Codex uses `/v1/responses`.
- Also has an **MCP server** so AI coding assistants can drive it.
- Browser UI at `http://127.0.0.1:8080` (Chat, live Monitor, About).

## Next

- Whether the GPU-VRAM-offload strategy is worth adopting for the V100/Intel
  swap path, or a smaller model is simpler.
- Runs on this box? RTX 3090 (24 GB) mentioned as a good fit — but this
  machine's GPU story is V100 + Iris Xe; check whether Strata's older-GPU
  support (P40/V100 listed) is good enough.

## Video

* "What's the Catch With Strata? My Friend's Setup Revealed It" by
  **Weights & Wonders** (2026-10-11).
  <https://www.youtube.com/watch?v=8kzSGQzGigc>
  Companion explainer on Strata's trade-offs (the "catch" on running a huge
  MoE on a consumer PC). See the Strata tag.
* ⚠️ **NOTE: check the transcript for interpretation.**

## Refs

- Paper: <https://github.com/Niko1221/Strata/blob/main/docs/paper/Strata-Paper.pdf>
- Details / models / benchmarks: `docs/DETAILS.md`, `docs/MODELS.md`,
  `docs/COMMUNITY_BENCHMARKS.md`
