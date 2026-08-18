# Chuwi Ubook XPro Camera Driver Plan

## 1. Goal
Adapt the generic V4L2 PCI skeleton driver to support two hardware camera modules (CAM0: IMX135, CAM1: OV2740) with proper hardware identification, I2C resource management, and sensor initialization.

## 2. Hardware Topology (from DSDT)
- **CAM0 (IMX135-CRDG2):** INT3471, I2C2 bus, I2C addr 0x0010, depends on PMIC
- **CAM1 (OV2740-CRDG2):** INT3474, I2C4 bus, I2C addr 0x0036, depends on I2C2.PMIC
- **PMIC (PMIC-CRDG2):** INT3472, I2C2 bus, I2C addr 0x004C

## 3. Implementation Status

### ✅ Completed
- **Device Abstraction:** `camera_device` struct with multi-device support
- **V4L2 Framework:** Video device and vb2 queue fully initialized
- **PMIC Power Control:** `pmic_check_and_enable()` for CAM0
- **Sensor Initialization:** `imx135_init()` and `ov2740_init()` implemented
- **Hardware Identification:** PCI device ID matching in `skeleton_probe`
- **I2C Resource Acquisition:** Proper I2C adapter lookup using bus IDs
- **PCI Table:** Updated with IMX135/OV2740 PCI device IDs

### 🚧 TODO (Next Steps)
1. **Refine IMX135 register map:** The current init uses basic register writes. Full implementation would include all timing, gain, and white balance registers from the IMX135 datasheet.
2. **Refine OV2740 register map:** The current init sets up basic streaming mode. Full implementation would include all ISP configuration, test patterns, and advanced controls.
3. **ACPI parsing:** Implement DSDT parsing for more robust device identification (currently relies on PCI IDs).
4. **DMA engine integration:** Connect the vb2 queue to the actual DMA engine for frame capture.
5. **Interrupt handling:** Implement full interrupt-driven frame capture.
6. **GPIO control:** Add GPIO-based camera control (privacy shutter, focus, etc.).

## 4. Midterm Goal (4 weeks)

**Make the camera driver functional end-to-end: proper sensor init, DMA frame capture, and basic V4L2 video streaming for both CAM0 (IMX135) and CAM1 (OV2740).**

Specific deliverables:
1. **Refined sensor register maps** — complete IMX135 and OV2740 init from datasheets (timing, gain, WB, ISP)
2. **Working DMA engine** — vb2 queue connected to real DMA for frame capture
3. **Interrupt-driven capture** — replace polling with interrupt-driven frame buffer management
4. **Video streaming** — `v4l2-ctl` can capture and stream frames from both cameras
5. **ACPI fallback** — DSDT parsing as a backup to PCI ID matching
6. **GPIO hooks** — privacy shutter control stub

## 5. Day-by-Day Breakdown

### Week 1 — Refine Sensor Init

| Day | Focus | Deliverable |
|-----|-------|-------------|
| Day 1 | IMX135 register map | Pull full IMX135 datasheet register map; wire all timing/gain/WB registers into `imx135_init()` |
| Day 2 | OV2740 register map | Pull full OV2740 datasheet; wire ISP config, test patterns, advanced controls into `ov2740_init()` |
| Day 3 | Test & verify sensor init | Compile, load module, verify both sensors report correct chip IDs via `v4l2-ctl --id=0` |
| Day 4 | PMIC power sequencing hardening | Add timeout + error recovery in `pmic_check_and_enable()`; log power state transitions |
| Day 5 | Refactor init functions | Clean up `imx135_init()` / `ov2740_init()` into helper functions (read_reg, write_reg, config) |
| Day 6 | Buffer management review | Audit `video_device_setup` / vb2 queue setup for correctness; fix any queue config issues |
| Day 7 | Week 1 review | Verify both cameras show up in `/dev/video*`, `v4l2-ctl --list-devices` shows correct formats |

### Week 2 — DMA Engine Integration

| Day | Focus | Deliverable |
|-----|-------|-------------|
| Day 8 | DMA engine research | Study `linux-source-6.8.0` DMA engine drivers for reference; pick a suitable engine |
| Day 9 | DMA driver selection | Choose camera DMA engine (e.g., `dw_msi` or platform-specific); write `camera_dma_attach()` |
| Day 10 | DMA buffer allocation | Implement DMA buffer pool in `camera_device` struct; link vb2 queues to DMA |
| Day 11 | DMA data path | Wire vb2 `start_mmap` / `start_buffer` to DMA engine; verify buffers get DMA addresses |
| Day 12 | Test DMA | Compile, load, verify DMA engine shows in dmesg; check frame sizes match sensor resolution |
| Day 13 | Error handling | Add DMA error recovery (re-attach, retry); log DMA errors |
| Day 14 | Week 2 review | Confirm DMA path works; `v4l2-ctl --stream-mmap` shows capture |

### Week 3 — Interrupts + Streaming

| Day | Focus | Deliverable |
|-----|-------|-------------|
| Day 15 | Interrupt framework | Implement `camera_device` interrupt handler; request IRQ in probe |
| Day 16 | Interrupt-driven capture | Replace polling in vb2 with interrupt-driven buffer completion; handle multiple frames |
| Day 17 | Frame DMA transfer | Implement DMA engine interrupt callback; transfer captured frames to vb2 buffers |
| Day 18 | Test streaming | `v4l2-ctl --stream-mmap --stream-to=test.bin` — capture test, verify frames in output |
| Day 19 | Frame format tuning | Verify output format matches sensor native resolution (1280×720 for IMX135, 1280×960 for OV2740) |
| Day 20 | Performance tuning | Tune buffer count, DMA burst size; reduce latency |
| Day 21 | Week 3 review | Live video capture working end-to-end for both cameras |

### Week 4 — ACPI, GPIO, Polish

| Day | Focus | Deliverable |
|-----|-------|-------------|
| Day 22 | ACPI DSDT parsing | Implement `acpi_camera_probe()` as fallback; parse INT3471/INT3474/INT3472 from DSDT |
| Day 23 | ACPI integration | Wire ACPI probe alongside PCI probe; prefer PCI, fall back to ACPI |
| Day 24 | GPIO stubs | Add privacy shutter GPIO control stub; add `video_export` for userspace control |
| Day 25 | Documentation | Document register maps, DMA configuration, ACPI table in `README.md` |
| Day 26 | Code cleanup | Remove dead code, fix any remaining warnings from `make` |
| Day 27 | Testing | Full integration test: both cameras, streaming, format verification |
| Day 28 | Week 4 review + milestone check | **Midterm complete** — driver functional end-to-end |

## 6. Key Functions
- `imx135_init()` - IMX135 sensor initialization (chip ID, firmware, mode, timing)
- `ov2740_init()` - OV2740 sensor initialization (chip ID, MIPI, streaming mode, timing)
- `pmic_check_and_enable()` - PMIC power state check and enable
- `imx135_pmic_read/write()` - PMIC I2C communication
- `ov2740_read_reg/write_reg()` - OV2740 register access

## 7. References
- DSDT: `./dsdt.cam0_cam1.dsl`
- OV2740 kernel driver: `./linux-source-6.8.0/drivers/media/i2c/ov2740.c`
- INT3472 PMIC driver: `./linux-source-6.8.0/drivers/platform/x86/intel/int3472/`
