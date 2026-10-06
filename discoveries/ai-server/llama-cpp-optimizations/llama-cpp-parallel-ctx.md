# Parallelism and Context

Goal:

* I want to have my user-facing pi-agent use sub-agents
* I want to have multiple sessions from eventually multiple devices

## Observation

Practically i experienced lots of insufficient tokenspace, when working in parallel.
At that time, kv_unified is on, and n_parallel was on 4 = default.
Compute-wise at least 2 processes where running fluently, while 2 more were kept on-hold by the appearances.

## Ressources

https://www.reddit.com/r/LocalLLaMA/comments/1sj0ebd/how_do_parallel_requests_share_context_size_on/
