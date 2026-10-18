# Local VLM (vision) input via llama.cpp

> **Status: HARDENING (2026-10-13)** — router-mode vision fix is verified
> working end-to-end (fresh pi session accepts images via crossbar). Upstream
> **PR #41** raised (open) from `janesser:fix/llamacpp-router-vision` →
> `Hypabolic:main`. Until merged/released, pi runs the branch pin.

## Situation

- **Symptom:** image attachments in pi were replaced with
  `(image omitted: model does not support images)` — although the default
  model is a VLM.
- **Server side was fine.** The router `cyberkleiber.fritz.box:8081`
  (router mode) reports per-model modalities on `/v1/models`:
  Ornith-1.5-35B (text+image, mmproj in the HF cache), Qwen3.8-27B and
  gemma-4-26B (text+image+video). Everything else: text.
- **Root cause: crossbar.** Its llamacpp adapter derived vision from the
  root `/props` — which is `modalities: null` in router mode — and ignored
  the per-model `architecture.input_modalities`. Every model was registered
  `input: ["text"]`; pi then dropped images. Upstream context: issue #35,
  PR #36 (root `/props` object-map fix, single-model servers only).

## Resolution (2026-10-13)

- Per-model fix implemented: commit `4cca16d` on
  `fix/llamacpp-router-vision` in `~/projs/crossbar` (checkout of
  Hypabolic/Crossbar + remotes `hwacookie` + `janesser`), built on PR #36
  head `65878e3`. Row `architecture.input_modalities` wins when present
  (explicit `["text"]` rows are not rescued by root props); root `/props`
  stays the fallback for single-model servers / older routers. 7 new tests,
  suite green (1 pre-existing environmental LAN-discovery failure).
- **PR #41** raised:
  https://github.com/Hypabolic/Crossbar/pull/41
- Deployed: pi package pin is now
  `git:github.com/janesser/Crossbar@fix/llamacpp-router-vision` (replaces
  the npm 0.8.0 and the earlier hwacookie PR-#36 pin).
- Verified: after a **fresh pi session** (session_start re-registers
  providers; in-session rescan/reselect does not rebind the ModelRegistry),
  Ornith-1.5-35B registers `input: ["text","image"]` and attached images
  reach the model.

## Next steps

1. Track PR #41; on merge/release: `pi install npm:@hypabolic/crossbar@<new>`
   (drop the git pin) and retest.
2. Pick the default model — Ornith-1.5-35B (vision) vs Qwen3.8-27B
   (vision+video).
3. Optional: verify the SYCL build's vision support before considering
   :8082 as a VLM host.
4. **Context-window reliability (separate thread, same fork):** v0.6.0's
   auto-fit makes `ctx-size` a per-load runtime value; the crossbar
   contextWindow chain and pi's compaction budget interact (the 128k-on-
   resume / 256k-fresh symptom). Decision record (local, git-ignored):
   `_bmad-output/planning-artifacts/architecture/architecture-llama-ctx-crossbar-2026-10-16/DECISIONS.md`
   — pinning dropped as obsolete; PR work = contextWindow knob with source
   labeling + resume re-probe + wait-for-load status.
