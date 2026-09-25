#!/bin/bash
# run_once_5_aitools_4llama_sycl_startup.sh
#
# NO-OP STUB.
#
# The SYCL (Intel) llama.cpp backend it used to provision (llama-sycl.service,
# :8082) is now folded into run_once_5_aitools_2llama_startup.sh, which manages
# BOTH backends from one backend table. This file is kept only so existing
# references (chezmoiscripts.dep.yml, docs) don't break; it exits 0 immediately.
# Remove it — and its dep.yml entry — once those references are updated.

echo "$(basename "$0"): no-op — SYCL backend is now provisioned by run_once_5_aitools_2llama_startup.sh."
