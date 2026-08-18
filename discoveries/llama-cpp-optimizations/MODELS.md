# Models Documentation

## Model Summary

| Model | Use-Case | Params+TPS | Comment |
|-------|----------|------------|---------|
| Ornith-1.0-9B | General-purpose, Qwen3.5-based | 8.95B / 2.52 tok/s | Optimized for RTX 2060 (6GB VRAM), Q4_K_M quantization, 5.23 GiB |

## Overview

**Model Name:** Ornith 1.0 9B  
**Repository:** deepreinforce-ai/Ornith-1.0-9B-GGUF  
**Quantization:** Q4_K_M (Q4_K - Medium)  
**GGUF Version:** V3 (latest)  
**File Size:** 5.23 GiB (5.02 BPW - Bits Per Weight)  
**Architecture:** Qwen3.5 (Qwen35)  
**File Type:** GGUF (Model)

## Hardware Configuration

### GPU (Primary)
- **GPU Model:** NVIDIA GeForce RTX 2060  
- **Total Memory:** 5737 MiB  
- **Free Memory (Initial):** 5644 MiB  
- **CUDA Architecture:** 750 (Turing)  
- **Driver:** CUDA-enabled

### CPU (Fallback)
- **Processor:** AMD Ryzen 5 4600H with Radeon Graphics  
- **Total Memory:** 15402 MiB  
- **Free Memory (Initial):** 15402 MiB

### System Details
- **Operating System:** Linux x86_64  
- **Build Version:** 10235 (221f0f635)  
- **Compiler:** GNU 13.3.0  
- **Threading:** 11 threads (HTTP server)  
- **Scheduling:** OpenMP enabled  
- **GPU Graphs:** Enabled (USE_GRAPHS = 1)

## Model Architecture Parameters

| Parameter | Value | Description |
|-----------|-------|-------------|
| `n_layer` | 32 | Number of transformer layers |
| `n_ctx` | 262,144 | Context window size (tokens) |
| `n_ctx_train` | 262,144 | Training context size |
| `n_embd` | 4,096 | Embedding dimension |
| `n_head` | 16 | Number of attention heads |
| `n_head_kv` | 4 | Number of KV heads (GQA) |
| `n_ff` | 12,288 | Feed-forward network dimension |
| `n_vocab` | 248,320 | Vocabulary size (tokens) |
| `n_merges` | 247,587 | BPE merge operations |
| `n_rot` | 64 | RoPE rotation dimensions |
| `n_gqa` | 4 | Grouped-query attention count |
| `n_embd_head_k` | 256 | KV head embedding dimension |
| `n_embd_v_gqa` | 1,024 | GQA embedding dimension |
| `rope_type` | 40 | RoPE type (linear scaling) |
| `freq_base` | 10,000,000 | RoPE frequency base |
| `rope_scaling` | linear | RoPE scaling type |
| `causal_attn` | 1 | Causal attention enabled |
| `ssm_d_conv` | 4 | State-space model convolution kernel |
| `ssm_d_inner` | 4,096 | State-space model inner dimension |
| `ssm_d_state` | 128 | State-space model state dimension |
| `ssm_dt_rank` | 32 | State-space model time step rank |
| `ssm_n_group` | 16 | State-space model group count |
| `tokenization` | BPE | Byte Pair Encoding |
| `chat_template` | peg-native | Native chat template |
| `reasoning` | 1 | Reasoning support enabled |

## Memory Allocation Strategy

### Device Memory (CUDA0)
- **Total GPU Memory:** 5737 MiB
- **Model Layers Allocated:** 10 layers (out of 32)
- **Model Memory Usage:** 4490 MiB
- **Remaining GPU Free:** 1051 MiB
- **KV Cache:** 864 MiB (CUDA) + 1440 MiB (CPU) = 2304 MiB
- **Compute Buffer:** 1655 MiB (CUDA) + 280 MiB (Host) = 1935 MiB
- **Total GPU Allocation:** ~5644 MiB

### Host Memory (CPU)
- **Total Host Memory:** 15402 MiB
- **Model Offload:** 3411 MiB (CPU-mapped)
- **KV Cache (CPU):** 1440 MiB
- **RS Buffer (CPU):** 75 MiB
- **Compute Buffer (CPU):** Included in total

### Memory Optimization Notes
- **Initial Fit Failed:** Projected 8553 MiB needed vs 5542 MiB available
- **Auto-Reduced Layers:** Successfully fit 10 layers on GPU
- **Fit Time:** 4.08 seconds
- **GPU Memory Limit:** 5644 MiB free (1051 MiB remaining after allocation)

## Performance Metrics

### Inference Performance
- **Prompt Processing Speed:** 404.24 tokens/second
- **Prompt Processing Time:** 69.08 seconds (27,924 tokens)
- **Prompt Evaluation Time:** 98,256.02 ms / 27,971 tokens
- **Average Eval Time:** 3.51 ms per token
- **Eval Speed:** 284.67 tokens/second
- **Decoding Speed:** 2.52 tokens/second
- **Graph Reuse:** 154 graphs reused (512 batch size)
- **Total Tokens Processed:** 28,127
- **Total Inference Time:** 159,772.61 ms (2.5 minutes)

### Sampling Configuration
- **Temperature:** 0.800
- **Top-K:** 40
- **Top-P (nucleus):** 0.950
- **Min-P:** 0.050
- **Typical-P:** 1.000
- **Repeat Penalty:** 1.000
- **Repeat Last N:** 64
- **Mirostat:** 0 (disabled)
- **XTC Probability:** 0.000
- **XTC Threshold:** 0.100
- **Reasoning Budget:** 2,147,483,647 tokens (unlimited)

## Token Usage

### Prompt Tokens
- **First Request:** 31,549 tokens
- **Second Request:** 27,971 tokens

### Generation
- **Tokens Decoded:** 151 tokens (per request)
- **Reasoning Tokens:** 0 (natural end)

### Special Tokens
- **BOS:** Token 11 (,)
- **EOS:** Token 248,046 (
</think>

)
- **PAD:** Token 248,044 (
</think>

)
- **EOT:** Token 248,046 (

)
- **FIM PRE:** Token 248,060 (
</think>

)
- **FIM SUF:** Token 248,062 (
</think>

)
- **FIM MID:** Token 248,061 (
</think>

)
- **FIM PAD:** Token 248,063 (
</think>

)
- **FIM REP:** Token 248,064 (
</think>

)
- **FIM SEP:** Token 248,065 (
</think>

)

## Cache and Performance Data

### KV Cache Configuration
- **KV Cache Size:** 2,304 MiB (262,144 cells, 8 layers, 2 sequences)
- **KV Cache K:** 1,152 MiB (q4_0 quantization)
- **KV Cache V:** 1,152 MiB (q4_0 quantization)
- **Context Checkpoints:** 32 checkpoints (50.251 MiB each)
- **Checkpointing:** Context checkpoints enabled (max 32, min spacing 8192)

### Tensor Loading
- **Total Tensors:** 427
- **F32 Tensors:** 177
- **Q4_K Tensors:** 217
- **Q6_K Tensors:** 33
- **Load Mode:** mmap
- **Layer Offloading:** 9 repeating layers to GPU
- **Output Layer:** Offloaded to GPU

## Model Metadata

- **Architecture Name:** qwen35
- **Model Type:** model
- **Model Name:** Ornith 1.0 9B
- **Base Name:** Ornith-1.0
- **Size Label:** 9B
- **Quantization Version:** 2
- **Tokenizer Model:** GPT2
- **Tokenizer Prefix:** qwen35

## Notes

- **CORS Warning:** CORS set to allow all origins (security risk)
- **SSL:** Running without SSL
- **Model Loading:** ~4.4 seconds for full model load
- **Graph Optimization:** Enabled with batch size 512
- **Flash Attention:** Enabled
- **KV Unified:** Enabled
- **Lightning Indexer:** Enabled
- **DeepSeek V4 HC:** Pre-enabled (chunked, fused)
- **Reasoning Support:** Enabled via chat template

## Performance Summary

| Metric | Value |
|--------|-------|
| **Decoding Speed** | 2.52 tokens/second |
| **Prompt Processing** | 404 tokens/second |
| **Total Inference** | 159.8 seconds (28,127 tokens) |
| **GPU Memory Usage** | 4,490 MiB / 5,737 MiB |
| **Model Size** | 5.23 GiB (Q4_K_M) |
| **Parameters** | 8.95 billion |

This model is optimized for constrained GPU environments (RTX 2060 with 6GB VRAM) using intelligent layer offloading to fit the 9B parameter model within available memory.
