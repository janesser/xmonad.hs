# Project Goal: Implement and align kernel camera drivers (IMX135/CAM0, CAM1) using DSDT specifications

# Phase 1: Analysis and Reference Acquisition
[] **Parse DSDT/ACPI Files:** Thoroughly analyze relevant DSDT files (e.g., `../dsdt.cam0_cam1.dsl`) to extract device identifiers (_ADR, _HID, _DDN), resource allocations (_CRS), and methods (_STA, _DSM).
[] **Identify Reference Implementations:** Find existing kernel modules (for kernel 7.0.0) or driver sources (e.g., from Raspberry Pi/Tegra) that support IMX135/CAM0 and CAM1.
[] **Acquire Target Driver:** Secure the reference source code (`cam0_ref_drv.c` or equivalent) for modification.
[] **Compile Reference Implementation:** Create 'Makefile' with a rule named 'compile_ref_impl' that does compile the reference implementation. Try compiling. Fix trivial errors otherwise abort.

# Phase 2: CAM0 Implementation (IMX135)
[] **Code Alignment:** Modify the reference driver source code to match the architecture and structure of the target system.
[] **DSDT Metadata Injection (CAM0):** Inject specific identifiers and configuration parameters from the DSDT file into the CAM0 driver logic (e.g., setting correct _ADR, _HID, and I2C bus parameters).
[] **Validation (CAM0):** Perform initial build and testing of the CAM0 kernel module to ensure it aligns with the DSDT definitions.
[] **Compile CAM0 Implementation:** Extend 'Makefile' with a rule named 'cam0_impl' that does compile the cam0 implementation. Make this the default rule. Try compiling. Fix all errors until compilation succeeds.
[] **Try modprobe:** Try loading the module. Assert there is a camera listed on 'cam -l'.
