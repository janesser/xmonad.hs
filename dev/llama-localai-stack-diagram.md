# Local-AI stack — how it all fits together (one scope)

Host: **cyberkleiber** (kernel 7.0.0-34, dual GPU: **V100** gpu0 + **DG1/Iris Xe** gpu1 `2f:00.0`, i915).

Everything below is one machine. Three OpenAI-compatible fronts, two inference backends, one driver.

```mermaid
flowchart TB
    subgraph pi["pi-agent (this box)"]
        P["defaultProvider: olla<br/>defaultModel: ornith.gguf (V100)"]
    end

    subgraph ollamaprovider["ollama provider plugin"]
        OLL["http://cyberkleiber:40114/olla/openai"]
    end

    P -->|OpenAI /chat/completions| OLL

    subgraph olla["olla.service :40114  (LLM proxy · routes by model)"]
        direction LR
        ROUT["route by model id"]
    end

    OLL --> ROUT

    # ---- CUDA backend (WORKING) ----
    subgraph cuda["llama-cuda.service :8081  ✅ WORKING"]
        CUDA["llama-server (CUDA build)<br/>serves ornith.gguf on V100"]
    end
    CUDA -->|NVML/CUDA| V100["V100 (gpu0)"]

    # ---- SYCL backend (BROKEN on --system) ----
    subgraph syclsvc["llama-sycl.service :8082  ❌ --system<br/>(self-healing restart, times out)"]
        SSCY["llama-server (SYCL build)<br/>LFM2.5-2.6B on DG1"]
    end
    SSCY -->|Level Zero + host driver| DG1a["DG1 (gpu1)"]

    ROUT --> CUDA
    ROUT --> SSCY

    # ---- localai :8080 ----
    subgraph localai["localai.service :8080  ❌ --system  · OpenAI front + orchestrator"]
        direction TB
        LM["model loader / RPC"]
        BE["backends dir ~/.local/share/localai/backends/"]
        CPUB["cpu-llama-cpp (CPU) ✅"]
        ISY["intel-sycl-f16-llama-cpp (SYCL) ❌<br/>run.sh → llama-cpp-rpc-server"]
        META["llama-cpp (meta pointer)"]
    end
    ROUT --> LM
    LM --> CPUB
    LM --> META
    LM --> ISY

    ISY -->|./run.sh sets ZIC_ENABLE_ALT_DRIVERS=.../libze_intel_gpu.so.1| DG1a
    LM -->|OpenAI :8080| CLIENTB["any OpenAI client / pi via :8080"]

    subgraph drv["host compute-runtime (installed by run_once_5)"]
        ZERO["libze_intel_gpu.so.1 1.14.37020<br/>(Level Zero / Intel SYCL driver for DG1)"]
        OLD["libze1 1.28.2 loader (was already present)"]
    end
    ZERO --> DG1a
    OLD --> DG1a
    ISY -.needs driver .-> ZERO
```

## One request, end to end

```mermaid
sequenceDiagram
    participant pi
    participant ollama as ollama provider (plugin)
    participant olla as olla.service :40114
    participant svc as llama-sycl.service :8082
    participant la as localai.service :8080
    participant be as intel-sycl backend (run.sh)
    participant gpu as DG1 / i915

    pi->>ollama: OpenAI POST /olla/openai (defaultModel = ornith.gguf)
    ollama->>olla: forward to :40114
    olla->>svc|svc: route by model id
    svc->>gpu: llama.cpp SYCL build + Level Zero + libze_intel_gpu.so.1
    gpu-->>svc: ❌ no device (falls back to CPU, then times out)

    alt pi uses :8080 instead
        pi->>la: OpenAI POST /v1/chat/completions
        la->>be: load model → ./run.sh
        be->>gpu: GGML_SYCL_DEVICE=0 + ZIC_ENABLE_ALT_DRIVERS
        gpu-->>be: works under <b>systemd --user</b> scope; ❌ under <b>systemd --system</b>
    end
```

## Backends (what lives under `~/.local/share/localai/backends/`)

| Backend | Accelerator | Launch | Status under `--system` |
|---|---|---|---|
| `cpu-llama-cpp` | CPU | localai native | ✅ works |
| `llama-cpp` | meta → SYCL | pointer | — |
| `intel-sycl-f16-llama-cpp` | **DG1 / Intel SYCL** | `./run.sh` → `llama-cpp-rpc-server` | ❌ no device; ✅ found DG1 under `systemd --user --scope` |
| `llama-cpp-grpc` / `llama-cpp-rpc-server` | (the actual llama.cpp proc) | spawned by run.sh | — |

`run.sh` is the key: it `exec`s the bundled `lib/ld.so`, and exports the bundled SYCL driver **only when `ZIC_ENABLE_ALT_DRIVERS` is unset** — so setting it to the host driver wins.

## The SYCL path, in one line

```
localai  →  intel-sycl-f16-llama-cpp/run.sh  →  llama-cpp SYCL build
            env: ZIC_ENABLE_ALT_DRIVERS=/usr/lib/x86_64-linux-gnu/libze_intel_gpu.so.1
                  GGML_SYCL_DEVICE=0
            →  Level Zero  →  host driver (libze-intel-gpu1)  →  DG1 (i915, 2f:00.0)
```

## Status summary

| Front | Port | SYCL/DG1? | `systemd --system` | `systemd --user` |
|---|---|---|---|---|
| llama-cuda | :8081 | V100/CUDA | ✅ | — |
| llama-sycl | :8082 | DG1/SYCL | ❌ no device / times out | — |
| localai | :8080 | DG1/SYCL | ❌ no device | ✅ **DG1 found** (subreaper refuted) |

**Bottom line:** SYCL works whenever the process is launched under the **user manager** (`systemd --user` scope), which is a subreaper — proving the driver, env, `ZIC_ENABLE_ALT_DRIVERS`, groups, and mount namespace are all fine. It fails only as a **`--system` service**. The remaining fix is to run localai (and/or the SYCL backend) under the user manager instead of a system unit.
