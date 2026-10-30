#!/bin/bash
# run_once_5_aitools_1llama_cpp.sh
#
# Build llama.cpp from source for the NVIDIA GPU this box has:
#
#   * NVIDIA GPU  -> CUDA build -> ~/projs/llama.cpp/build_cuda   (built here)
#
# The CUDA build runs during `cz apply`.
#
# The per-backend launcher command is written to ~/.local/bin:
#   llama-server-cuda   # -> build_cuda   (CUDA_VISIBLE_DEVICES)
#
# Root is only ever used for commands allowed by the scoped NOPASSWD sudoers
# drop-in (see AGENTS.md): apt, curl, mkdir, chmod, cp, mv, tee.

set -o pipefail
export DEBIAN_FRONTEND=noninteractive

# SAFEGUARD: apt-only script. The CUDA build dependencies are apt packages
# (`glslang-dev`, `nvidia-cuda-toolkit`, ...) — on a pacman/yay box they cannot be
# installed, so skip silently instead of failing the whole `cz apply`.
command -v apt >/dev/null || exit 0

log() { echo "$(basename "$0"): $*"; }

LLAMA_REF="v0.8.0"
log "Building $LLAMA_REF"


source "${CHEZMOI_SOURCE_DIR:-$HOME/.local/share/chezmoi}/commons/mise_assert-uv.sh"
uv tool install gpustat

# --- paths ------------------------------------------------------------------
LLAMA_DIR="$HOME/projs/llama.cpp"
BUILD_CUDA="$LLAMA_DIR/build_cuda"
NCPUS="$(grep -c '^processor' /proc/cpuinfo 2>/dev/null || echo 4)"

source "${CHEZMOI_SOURCE_DIR:-$HOME/.local/share/chezmoi}/commons/gpu-detect.sh"

# --- builders ---------------------------------------------------------------
# NOTE: the source dir is passed EXPLICITLY to cmake (-B build $LLAMA_DIR) so a
# stale/pwd-independent CMakeLists lookup never points at the wrong folder.
build_cuda() {
    local ref="$1"
    fetch_llama_cpp "$ref"
    log "Installing CUDA build dependencies..."
    sudo apt install -y glslang-dev glslc spirv-headers libssl-dev libnccl-dev ccache nvidia-cuda-toolkit
    log "Building llama.cpp (CUDA) -> $BUILD_CUDA"
    cmake -B "$BUILD_CUDA" "$LLAMA_DIR" -DGGML_CCACHE="${CACHE:-ON}" -DGGML_CUDA=ON \
        -DLLAMA_BUILD_TESTS=OFF -DLLAMA_BUILD_EXAMPLES=OFF -DLLAMA_BUILD_UI=OFF \
        -DLLAMA_BUILD_SERVER=ON
    cmake --build "$BUILD_CUDA" --config Release -j "$NCPUS"
}

# clone/fetch/checkout llama.cpp (idempotent; reuses an existing checkout)
fetch_llama_cpp() {
    local tag="$1"
    if [ ! -d "$LLAMA_DIR/.git" ]; then
        rm -rf "$LLAMA_DIR"
        git clone https://github.com/ggml-org/llama.cpp "$LLAMA_DIR"
    fi
    if ! ( cd "$LLAMA_DIR" && git fetch --all --prune && git checkout "$ref" ); then
        log "ERROR: could not fetch/checkout '$ref' in $LLAMA_DIR"
        return 1
    fi
}

# --- generated shared lib + launchers ---------------------------------------
write_generated_files() {
    mkdir -p "$HOME/.local/bin"


    # CUDA launcher: point at the pre-built CUDA tree. Uses the dedicated
    # 'llama-server' binary (not the unified 'llama') so --cache-list works.
    cat > "$HOME/.local/bin/llama-server-cuda" <<'EOF_CUDA'
#!/bin/bash
# llama.cpp on the NVIDIA GPU (CUDA build). Passthrough: append serve/--cache-list
# etc, e.g.  llama-server-cuda --cache-list  or  llama-server-cuda serve -m ...
export CUDA_VISIBLE_DEVICES="${CUDA_VISIBLE_DEVICES:-0}"
LLAMA="$HOME/projs/llama.cpp/build_cuda/bin/llama-server"
if [ ! -x "$LLAMA" ]; then
    echo "llama-server-cuda: CUDA build not available at $LLAMA — run the build once." >&2
    exit 1
fi
# dedicated server binary: it (not the unified 'llama') exposes --cache-list
exec "$LLAMA" "$@"
EOF_CUDA


    chmod +x "$HOME/.local/bin/llama-server-cuda"
    log "wrote ~/.local/bin/llama-server-cuda"
}

# --- main -------------------------------------------------------------------
if ! has_nvidia; then
    log "No CUDA-capable GPU detected — nothing to build."
    exit 0
fi

# --- backend selection (configurable) --------------------------------------
# LLAMA_BACKENDS is a space-separated list of backends to build ("cuda"). It
# defaults to what the box actually has; override it in the environment to
# force a subset.
if [ -z "${LLAMA_BACKENDS}" ]; then
    _auto=""; has_nvidia && _auto="cuda"
    LLAMA_BACKENDS="${_auto:-}"
fi
want_backend() {
    local b="$1" x
    for x in ${LLAMA_BACKENDS}; do [ "$x" = "$b" ] && return 0; done
    return 1
}

# --- CUDA (built here, during apply) ----------------------------------------
if want_backend cuda; then
    build_cuda "$LLAMA_REF" || log "CUDA build failed — investigate above and re-run."
fi

# --- generate launcher --------------------------------------------------------
write_generated_files

log "Done. Backend-specific commands:"
log "  llama-server-cuda   # NVIDIA / CUDA  (build_cuda)"
# NOTE: the boot service (llama-cuda) is CUDA-only by design.
#
# CUDA init error on suspend/resume (fixed upstream):
# https://github.com/ggml-org/llama.cpp/issues/7218
