# AGENT.md Additions — Folder Simplification

Add these rules to the existing AGENT.md:

## New Rules

10. Use agent_work/ subdirectories by concern:
    - `docs/` for day logs, research, READMEs
    - `src/` for source code (.c, .h, .diff)
    - `build/` for compiled objects (.o, .mod, .ko)
    - `scripts/` for check/scan/helper scripts
    - `acpi/` for ACPI patches, DSDT files
    - `config/` for Makefiles, .gitignore

11. Write compiled artifacts to `agent_work/build/`

12. Write source code to `agent_work/src/`

13. Write scripts to `agent_work/scripts/`

14. Write day logs to `agent_work/docs/` with date prefix (e.g., `day_20240729.md`)

15. Add `.gitignore` files to each subdirectory:
    - `build/` → *.o, *.mod, *.ko
    - `scripts/` → *.log
    - Root `agent_work/` → *.log

16. Before restructuring, request a refreshed view of current files

## Preserved Rules (1-9 unchanged)
1. Check README.md and PLAN.md
2. Consider project directory read-only, write to agent_work directory
3. Use grill-me-with-docs before engaging
4. Do very small steps at a time
5. Create handovers in agent_work to revert to it if required
6. Carefully parse error output to pinpoint exact problems
7. Whenever pointing to files, consider path in current working directory first
8. Check every path against ./agent_work/fs_mappings.md, prefer softlinks
9. When sudo is needed, ask the user to execute the command for you
