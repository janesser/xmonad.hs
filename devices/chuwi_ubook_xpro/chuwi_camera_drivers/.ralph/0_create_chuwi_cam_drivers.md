
# Project Goal: Implement and align kernel camera drivers (IMX135/CAM0, CAM1) using DSDT specifications

# Phase 1: Analysis and Reference Acquisition
1.  **Parse DSDT/ACPI Files:** Thoroughly analyze relevant DSDT files (e.g., `../dsdt.cam0_cam1.dsl`) to extract device identifiers (_ADR, _HID, _DDN), resource allocations (_CRS), and methods (_STA, _DSM).
2.  **Identify Reference Implementations:** Find existing kernel modules or driver sources (e.g., from Raspberry Pi/Tegra) that support IMX135/CAM0 and CAM1.
3.  **Acquire Target Driver:** Secure the reference source code (`cam_driver.c` or equivalent) for modification.

# Phase 2: CAM0 Implementation (IMX135)
4.  **Code Alignment:** Modify the reference driver source code to match the architecture and structure of the target system.
5.  **DSDT Metadata Injection (CAM0):** Inject specific identifiers and configuration parameters from the DSDT file into the CAM0 driver logic (e.g., setting correct _ADR, _HID, and I2C bus parameters).
6.  **Validation (CAM0):** Perform initial build and testing of the CAM0 kernel module to ensure it aligns with the DSDT definitions.

# Phase 3: CAM1 Implementation
7.  **Code Alignment:** Apply the same alignment process to the CAM1 reference driver (if different) or extend the CAM0 driver.
8.  **DSDT Metadata Injection (CAM1):** Inject specific identifiers and configuration parameters from the DSDT file into the CAM1 driver logic (e.g., setting correct _ADR, _HID, and resource mappings for CAM1).
9.  **Validation (CAM1):** Perform initial build and testing of the CAM1 kernel module.

# Phase 4: Finalization and Integration
10. **Integration & Testing:** Perform comprehensive system-level testing with both CAM0 and CAM1 active to ensure smooth operation and compatibility.
11. **Documentation:** Document the final structure, all modifications made, and the rationale behind DSDT-to-driver injection decisions.
