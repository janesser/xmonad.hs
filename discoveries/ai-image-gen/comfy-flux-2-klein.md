# ComfyUI + Flux-2-Klein image generation (NVIDIA)

> ⚠️ **Superseded by Qwen-Image-2.1.** This is the *older* ComfyUI / multi-file
> Flux-2-Klein approach. Kept here as historical handover; the current
> direction is `qwen-image-21.md`.

Status: **documented, script disabled pending solidify.** The command sequence
below was lifted from `.chezmoiscripts/run_once_5_aitools_4image_gen.sh`
(the `exit 0 # FIXME solidify` guard made it a no-op). It is recorded here as
handover documentation; the source script has been removed.

Related: the current target, `discoveries/ai-image-gen/qwen-image-21.md`.

---

## TL;DR

Install **ComfyUI** locally on an NVIDIA box via `comfy-cli`, restore the
`Flux-2-Klein` model chain from HuggingFace, soft-link the files into the
ComfyUI `models/` tree, drop in the text-to-image workflow, then launch.

All of this is gated behind an NVIDIA-presence check so it never runs (or
installs anything) on a non-NVIDIA machine.

---

## 1. NVIDIA presence guard

The whole script skips itself unless an NVIDIA GPU is visible. Uses `lspci`
(not `lsmod`) because `lspci` needs no sudo and detects the GPU even when the
driver module is not loaded — `lsmod` would false-negative on a fresh setup.

```bash
if ! lspci 2>/dev/null | grep -iq nvidia; then
    echo "$(basename $0): No NVIDIA GPU detected, skipping..."
    exit 0
fi
```

---

## 2. Environment & comfy-cli

```bash
uv venv --allow-existing ~/.comfy
source ~/.comfy/bin/activate
uv pip install comfy-cli
```

---

## 3. ComfyUI setup & install

```bash
comfy setup --where local -y
cd ~/.comfy
comfy install --restore --nvidia --cuda-version 12.4 --fast-deps --version=latest

comfy manager enable-gui
comfy manager uv-compile-default true
```

- `--cuda-version 12.4` targets CUDA 12.4.
- `--restore` reinstalls the previously saved model chain.

---

## 4. Flux-2-Klein model download

Docs: <https://docs.comfy.org/tutorials/flux/flux-2-klein>

```bash
uvx hf download hf://Comfy-Org/vae-text-encorder-for-flux-klein-4b/split_files/diffusion_models/flux-2-klein-base-4b.safetensors
uvx hf download hf://Comfy-Org/vae-text-encorder-for-flux-klein-4b/split_files/diffusion_models/flux-2-klein-4b.safetensors

uvx hf download hf://Comfy-Org/vae-text-encorder-for-flux-klein-4b/split_files/text_encoders/qwen_3_4b.safetensors
uvx hf download hf://Comfy-Org/vae-text-encorder-for-flux-klein-4b/split_files/text_encoders/qwen_3_4b_fp4_flux2.safetensors

uvx hf download hf://Comfy-Org/vae-text-encorder-for-flux-klein-4b/split_files/vae/flux2-vae.safetensors
```

(The two `diffusion_models` lines are duplicated in the original script.)

---

## 5. Soft-link downloaded files into ComfyUI models

Each file under the HuggingFace snapshot is soft-linked into the matching
`~/.comfy/ComfyUI/models/<subfolder>` directory:

```bash
for folder in ~/.cache/huggingface/hub/models--Comfy-Org--vae-text-encorder-for-flux-klein-4b/snapshots/*/split_files/*; do
    basefolder=`basename $folder`
    for item in `ls $folder`; do
        cd ~/comfy/ComfyUI/models/$basefolder
        ln -sf "$folder/$item"
    done
done
echo all linked nicely.
```

---

## 6. Workflow download

```bash
curl -o ~/comfy/ComfyUI/user/default/workflows/image_flux2_klein_text_to_image.json \
  https://raw.githubusercontent.com/Comfy-Org/workflow_templates/refs/heads/main/templates/image_flux2_klein_text_to_image.json
```

---

## 7. Launch

```bash
# comfy launch (command was not captured beyond this comment)
```
