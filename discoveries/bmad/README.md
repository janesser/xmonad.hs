# Spec Driven Ai Tools

<https://ranthebuilder.cloud/blog/i-tested-three-spec-driven-ai-tools-here-s-my-honest-take/>

## BMAD

### Installation with pi-agent

    npx bmad-method install --directory . --modules bmm --tools pi --yes

### Marsrover example

    # model used ornith-ai/Ornith-1.5-35B-A3B-GGUF:Q4_K_M
    pi> /skill:bmad-build write an implementation of mars rover kata

### Struggle: Context Window

Context-window will soon reach auto-compaction and then turn into some start/stop.

1. Tweaked ctx-size in llama-cpp
2. Activated rope yarn (is this effective?)
