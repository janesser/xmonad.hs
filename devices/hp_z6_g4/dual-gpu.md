# Dual GPU (NVLink, 2× V100 SXM2 32GB) — HP Z6 G4

**Status: held (2026-10-23)** — parked design space, distilled from the forge
session `vllm-dual-gpu-migration` (memlog: `_bmad-output/forge/vllm-dual-gpu-migration/.memlog.md`).
Era 1 (single SXM2 on cyberkleiber, little-brother on the PCIe V100) is the
active path; revisit this when the second SXM2 is on order or installed.

**Assumption (per Jan, 2026-10-23):** a NVLink-connected 2× V100-SXM2-32GB
situation exists "some day". All numbers below are planning-grade, to be
re-benchmarked on the real chassis.

---

## 1. What the second card is actually for

The 27B (Qwen3.8-27B, Q4_K_M ≈ 17 GB) **fits on one card** — so the dual GPU
is not a speed multiplier (knightli.com, 2026-05-09: llama.cpp multi-GPU is
"mainly a VRAM and placement tool, not an automatic speed multiplier"). The
dual SXM2 buys three things, in order of value for this workload:

1. **A dedicated card for the little brother + sd.cpp.** Card 2 hosts the
   small auxiliary model (24/7) and the Flux.2-klein image pipeline. The
   switchmode (evicting the 27B to borrow the V100 for image batches) dies
   entirely — as does the mutual-support handover protocol in AGENTS.md
   (its purpose: keep a text backend answering while `llama-cuda` is down).
   A permanent card-2 text backend makes the handover trivially true.
2. **A 64 GB KV pool (TP=2).** Qwen3.8-27B is a *hybrid* linear-attention
   model (48 Gated-DeltaNet layers + 16 Gated-Attention layers; see HF card
   for unsloth/Qwen3.8-27B-GGUF). KV grows only in the 16 full-attention
   layers: ≈ 64 KB/token fp16 (≈ 32 KB q8). Four 32k-token sessions ≈ 9 GB
   KV — fits on ONE card. TP=2 buys headroom for 128k+ contexts, or the 27B
   at Q8/FP16 (≈ 35–54 GB weights) for quality, or both.
3. **Prefill/batch throughput.** Tensor-parallel work pays mainly on strong
   interconnects (NVLink ≫ PCIe Gen3's 32 GB/s, per the same article).
   Single-user decode gains less than prefill and concurrent serving.

## 2. Engine choice: where vLLM's advantage is real

- **vLLM TP=2 over NVLink** is its best-engineered path: PagedAttention KV
  accounting across 64 GB, continuous batching, automatic prefix caching
  (shared system prompts across 4 agent sessions re-prefill once), MTP
  speculative decode (Qwen3.8-27B is trained with MTP heads — vLLM has an
  MTP path; see v100-vllm-2026 Ch. 4).
- **llama.cpp** also runs on NVLink (article recommends testing
  `--split-mode tensor` on SXM2/NVLink; start at `--split-mode layer`), but
  its KV pre-allocation (`--kv-unified --parallel N`) is coarser, and it has
  no cross-request prefix caching or MTP serving path.

**The V100 catch (verified 2026-10-23):** prebuilt vLLM wheels/images dropped
sm_70 at 0.21 (CUDA ≥12.8 arch lists start at sm_75); "no kernel image" on
Volta. Viable paths:
- vLLM **0.19/0.21 source-built on CUDA 12.6** (CMake `<12.8` branch still
  lists sm_70) — demonstrated on 8× V100-32GB by
  KumphanartDansiri/v100-vllm-2026 (frozen matrix), which includes model
  pages for **Qwen3.5-27B and Qwen3.6-27B** (same config family) + an FP8
  W8A16 sm70 plugin.
- 1CatAI/1Cat-vLLM (1.2.1, "Make Volta Fast Again", built on vLLM 0.21.1rc,
  claims SM70 + "Qwen hybrid model" support).
- Volta quant limits: GPTQ ✅, FP16 ✅, AWQ/FP8/Marlin ❌ (stock); GGUF via
  loader (dequant path) — to verify per stack.

## 3. VRAM planning table (planning-grade)

| Config | Weights | KV (4 sessions) | Fits |
|---|---|---|---|
| 27B Q4_K_M, 1 card, 4×32k ctx | ~17 GB | ~9 GB (fp16 KV) | ✅ 32 GB (≈26 GB used) |
| + little brother (~1–2 GB) co-resident | ~18–19 GB | ~9 GB | ✅ tight (≈28 GB) |
| 27B Q4, TP=2, 4×128k ctx | ~17 GB / 2 cards | ~36 GB (fp16) | ✅ 64 GB |
| 27B FP16, TP=2, 4×32k ctx | ~54 GB | ~9 GB | ✅ 64 GB (≈63 GB — minimal headroom) |
| 27B Q8, TP=2, 4×64k ctx | ~35 GB | ~18 GB | ✅ 64 GB |

KV per token ≈ 64 KB fp16 / 32 KB q8, derived from the 16 full-attention
layers (4 KV heads × 256 dim × 2 × 2 bytes); DeltaNet recurrent state is
≈ fixed per sequence (order 100 MB), not per token. **Verify against the
real config.json before committing to a context budget.**

## 4. Open items (kill-shots to check before buying/committing)

1. **Chassis feasibility.** V100-SXM2 needs an SXM2 baseplate; NVLink must be
   physically bridged (NVLink does **not** pass through passive SXM2→PCIe
   risers). Confirm the Z6 G4 (or whatever the final box is) can host two
   baseplates *and* the NVLink bridge cabling. If the bridge can't be wired,
   this degrades to "2× 32 GB over PCIe" — which is the VRAM-pool case only,
   and the engine choice (vLLM TP vs llama.cpp layer-split) should be
   re-tested accordingly.
2. **Qwen3.8-27B on sm70 vLLM.** 3.5/3.6-27B are demonstrated in the v100
   write-up; 3.8 (newer, hybrid + MTP + vision) is **not yet verified** in
   either stack. First bench on arrival: does it load, does MTP work, pp512/
   tg128 vs the llama.cpp baseline on the same card.
3. **PCIe V100 fate — RESOLVED (2026-10-23):** the PCIe card stays for the
   whole of Era 1 and is *replaced by a second SXM2* in Era 2 (same slot,
   same box). The card-2 services (little brother + sd.cpp) survive the swap;
   re-pin the UUID in `llama-little-brother.service` and card 2 gains NVLink
   for free. See `../ai-server/little-brother-card2-swap.md`.
4. **vLLM sleep mode on sm70** (weights→RAM swap for model changes) — only
   interesting if model-swap latency ever hurts; the box has ~19 GB free RAM,
   marginal for a 17 GB sleep-resident.

## 5. Sources

- Live cyberkleiber state 2026-10-22/23 (nvidia-smi, unit files, router
  cmdline) — see forge memlog.
- HF model card: unsloth/Qwen3.8-27B-GGUF (hybrid arch, 262k ctx, MTP).
- https://knightli.com/en/2026/05/09/llama-cpp-multi-gpu-offload-performance/
- https://github.com/KumphanartDansiri/v100-vllm-2026 (+ fp8-w8a16-sm70)
- https://github.com/1CatAI/1Cat-vLLM (v1.2.1)
- vLLM docs/forums on sm_70 drop (0.20+/0.21, CUDA 12.8+).
- Forge session: `_bmad-output/forge/vllm-dual-gpu-migration/`
