# Graph Databases for Local, Offline, Portable AI Agents

**Handover — research summary**
Date: 2026-08-08
Scope: comparing graph database options for an on-device, local-AI,
local-network, offline-capable, portable setup (llama.cpp at boot,
chezmoi-managed dotfiles, no cloud dependency).

---

## Why the HydraDB article is not the answer

The source article ([hydradb.com/blog/graph-databases-ai-agents](https://hydradb.com/blog/graph-databases-ai-agents))
is vendor content: HydraDB ranks itself first, and most of its ranked
options (HydraDB, Zep, Amazon Neptune, TigerGraph, NebulaGraph) are
cloud/managed or cluster-only. They fail the local + offline + portable
constraints outright. Treat the article as a landscape survey, not a
recommendation.

The real selection axis is **deployment model**, not "graph vs vector".

---

## Constraints that drive the choice

- No cloud dependency → eliminates managed GraphRAG SaaS.
- Single machine, no cluster → eliminates distributed graph DBs.
- Portable between computers → prefers file-based / embeddable over
  server-install.
- No permanent connection → no remote embeddings / license / sync calls.
- Local AI (llama.cpp) → embeddings generated locally, zero API keys.

---

## Candidates ranked for local + portable + offline

### 1. grit — single SQLite file, bi-temporal, concurrent merge ✅ (winner)
- One SQLite file, in-process, Rust, no server/daemon/network. MIT/Apache-2.0.
- Layer 1 of the "pearl" stack (grit → nacre → app); nacre is the LLM
  extraction pipeline, app is Layer 3.
- Bi-temporal: `as_of`/`as_at` time travel is a query.
- Hybrid retrieval: BM25 + exact vector scan (sqlite-vec) + graph
  expansion, RRF-fused. Deterministic (no ANN approximation).
- Embeddings are "recomputable local state" — bring your own embedder
  (llama.cpp embedding endpoint, fastembed, sentence-transformers).
- **Concurrent evolution:** idempotent + commutative ops, HLC + UUIDv7
  ids, last-writer-wins per field, property-tested convergence,
  `apply_remote` for cross-device op-log sync. The only option here
  built for this. See below.
- Limitation: exact vector scan is O(corpus) — ~25k dim-1024 vectors in
  50ms on Apple Silicon. Fine for agent memory, not for 1M-doc RAG.
- https://github.com/bofeizhu/grit

### 2. FathomDB — local-first datastore on SQLite
- SQLite (FTS5 + sqlite-vec), in-process. Python/TS/Rust SDKs + CLI.
  MIT. Pre-1.0 beta (0.8.x), very active.
- Hybrid retrieval: vector + FTS5 RRF-fused, optional cross-encoder
  rerank + optional graph-BFS arm. Optional in-process embedder
  (`bge-small-en-v1.5`, pure Rust).
- Bitemporal-ish: supersession, world-time validity windows.
- **Does not** support concurrent evolution: single-writer-thread model.
- https://github.com/coreyt/fathomdb

### 3. LadybugDB — Kuzu successor, embedded columnar graph
- Embedded (directory-based), columnar disk storage, Cypher. MIT.
- Most serious continuation of KuzuDB (archived after Apple acquisition);
  repositioned for agentic memory. Vector + full-text.
- Not single-file portable (copy the directory). No CRDT/merge.
- https://github.com/LadybugDB/ladybug

### 4. sqlite-graphrag — minimalist GraphRAG in one binary
- Single SQLite file, Rust, ~25 MB binary. Zero external services.
- Can embed locally (fastembed + multilingual-e5-small).
- Narrow scope (GraphRAG memory, not a general graph DB). No merge.
- https://github.com/daniloaguiarbr/sqlite-graphrag

### 5. Neo4j Community — mature but heavy fallback
- Server-based JVM, not embeddable. GPL-3.0.
- Best ecosystem (Cypher, Graph Data Science, vector indexes). Can run
  fully local.
- Fails "portable" and "minimal": needs JVM, RAM (~1GB+ for OS alone,
  more for page cache), install + service. No cluster topology in
  Community; `MERGE` is idempotent upsert, not dedup/merge.
- https://neo4j.com/docs/operations-manual/

## Not a fit for this project (and why)

- **HydraDB, Zep, Amazon Neptune, TigerGraph, NebulaGraph** — cloud /
  managed / cluster-only.
- **Memgraph** — in-memory (working set must fit in RAM), BSL license.
- **FalkorDB** — Redis-based; self-hostable but needs Redis running.
- **Original KuzuDB** — archived / acquired by Apple. Use LadybugDB.

---

## Concurrent evolution / merge

**Only grit supports this** among the candidates:
- Idempotent + commutative ops → converge to the same graph regardless of
  order.
- HLC + UUIDv7 → deterministic collision resolution (lowest HLC wins on
  create; last-writer-wins per metadata field).
- `MergeNodes` op (dedup, cycle-safe canonical resolution); grit only
  *scores* candidates (`find_merge_candidates`), you decide the merge.
- `apply_remote` for cross-device oplog sync.
- Property-tested across adversarial interleavings (merge cycles,
  purges racing adds, out-of-order invalidations).
- Sync unit = the op-log; JSONL export/import ships between machines →
  converges when offline devices meet.

FathomDB is single-writer; Neo4j Community uses transaction locking with
no CRDT/merge; sqlite-graphrag and LadybugDB have no merge semantics.

---

## Recommended architecture (for local llama.cpp)

1. Generate embeddings locally — point your graph tool at the local
   llama.cpp embedding endpoint, or use an in-process embedder
   (fastembed / bge-small). Zero API keys, no network egress.
2. Use grit as the graph + hybrid-retrieval store. It is designed for
   on-device, sovereign, offline agents and is the only candidate with
   deterministic concurrent/merge evolution.
3. Portability = ship the single SQLite file (grit's op-log) between
   machines; converge deterministically when devices reconnect.
