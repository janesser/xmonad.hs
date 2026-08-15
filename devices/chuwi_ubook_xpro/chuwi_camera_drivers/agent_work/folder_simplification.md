# Folder Simplification Handover

## Goal
Make `agent_work/` the common root for all matter (camera driver, DptfAnalysis, snap_ghidra).

## Current Structure
```
agent_work/
├── day1.md through day12.md     # Day logs (flat)
├── tps68470.c, .h, .o, .mod      # Source + compiled objects (mixed)
├── dsdt.patched.*                # ACPI patches (at root)
├── check_*.sh, try_*.sh          # Scripts (at root)
├── Makefile                      # Build config (at root)
├── archive/                      # Old code (subdir)
└── fs_mappings.md                # Softlink mappings
```

## Proposed Structure
```
agent_work/
├── docs/             # Day logs, research findings, READMEs
├── src/              # Source code (.c, .h, .diff)
├── build/            # Compiled objects (.o, .mod), kernel modules (.ko)
├── scripts/          # Check/scan/helper scripts
├── acpi/             # ACPI patches, DSDT files
└── config/           # Makefiles, .gitignore
```

## Simplified AGENT.md Additions
- Rule 10: Write to agent_work/ subdirectories by concern
- Rule 11: Compiled artifacts → agent_work/build/
- Rule 12: Analysis binaries → agent_work/compiled/
- Rule 13: Source code → agent_work/src/
- Rule 14: Scripts → agent_work/scripts/
- Rule 15: Day logs → agent_work/docs/ with date prefix
- Rule 16: Add .gitignore for each subdirectory
- Rule 17: When producing build artifacts, write to agent_work/build/
- Rule 18: Analysis projects have master README at root

## Action Required
Please confirm the current file listing before I proceed with restructuring.

## Next Steps
1. List all current files in agent_work/
2. Confirm file locations and contents
3. Execute the restructure
4. Update AGENT.md with simplified rules
5. Add .gitignore files
