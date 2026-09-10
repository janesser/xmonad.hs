// SPDX-License-Identifier: GPL-2.0-only
/*
 * scc_camera_proto.c  --  PROTOTYPE only. NOT for installation on a daily driver.
 *
 * Goal: reproduce the Windows "SkcController.sys" camera power-on sequence from Linux
 *       by emulating the SCC (Intel "Control Logic",aka SCC) Super-I/O register bank
 *       over I/O ports. This is a reference/prototype module, not a production driver.
 *
 * Source of truth: agent_work/scc_data/SCC_register_map.md (extracted from SkcController.sys
 *       by the Windows Ghidra run, Ghidra 12.1.3). Sequence = SSCrdG2TiSensor::SensorPowerOn
 *       (front OV2680, param_3 == 5).
 *
 * KNOWN (from static RE):  register indices, the fixed 0x8a rail-enable writes, the
 *                          MCLK enable/rate registers, the write ORDER.
 * RUNTIME-GAP  (must fill from kernel WinDbg, see TODO_port_and_power_values.md):
 *                          - the exact I/O port (the register writer calls a runtime
 *                            injected HAL (*HAL[0x14001d7b0]); the port lives there)
 *                          - the byte-record encoding of each write (1/2/4-byte)
 *                          - the config-derived voltage-rail decode + MCLK rate field
 *
 * If you are reading this and have not filled the RUNTIME-GAP markers below, this module
 * will compile but will NOT power the camera correctly. Do not load it until then.
 */

#include <linux/module.h>
#include <linux/io.h>          /* outb/outw/outl, request_region */
#include <linux/moduleparam.h>

/* ------------------------------------------------------------------ *
 * CONFIG:  fill the [RUNTIME-GAP] slots from kernel WinDbg before use *
 * ------------------------------------------------------------------ */

/* [RUNTIME-GAP] The SCC register bank I/O port (%dx in SkcController). Unknown statically. */
static unsigned int scc_port = 0;                    /* 0 = unset -> module refuses to load */
module_param(scc_port, uint, 0444);
MODULE_PARM_DESC(scc_port, "SCC register-bank I/O port (runtime-gate: confirm via WinDbg)");

/* [RUNTIME-GAP] Byte-width per register access. SkcController picks 1/2/4 bytes by an
 * access-type param; we do NOT know which. 1 = outb (byte) is the common case for these
 * control regs; verify against the injected HAL before trusting. */
static unsigned int scc_width = 1;                   /* 1, 2 or 4 */

/* [RUNTIME-GAP] Camera power-rail voltages are DECODED at runtime from a config struct
 * (decode (V-875)/17.8, clamped to &0x7f; VPP (V-900)/25). The exact byte per rail depends
 * on the board config and is NOT a fixed constant. Fill from the running config blob.
 * The table below indexes by the register number used in the power-on sequence. */
static const u8 scc_rail_value[0x46] = {
	/* idx:  0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f */
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, /* 0x0-0xf */
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, /* 0x10-0x1f */
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, /* 0x20-0x2f */
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, /* 0x30-0x3f */
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, /* 0x40-0x4f */
};

/* ------------------------------------------------------------------ *
 * Port I/O layer                                                     *
 * ------------------------------------------------------------------ */
/*
 * Legacy I/O ports need privilege: a kernel module runs in ring 0 and can use
 * outb()/outw()/outl() directly (after request_region()), OR a userspace helper
 * calls iopl(3) then __outb(). This prototype uses the kernel-module path.
 */
static int scc_port_open(void)
{
	if (!scc_port) {
		pr_err("scc: scc_port unset ([RUNTIME-GAP]) - refusing to load\n");
		return -EINVAL;
	}
	if (!request_region(scc_port, 4, "scc_proto")) {
		pr_err("scc: I/O region 0x%x unavailable\n", scc_port);
		return -EBUSY;
	}
	return 0;
}

static void scc_port_close(void)
{
	release_region(scc_port, 4);
}

/* Write a register: (port, index, value, width). Index is written to %dx by the
 * SkcController accessor; we model the injected HAL here as a direct port write
 * of the (byte-encoded) value. [RUNTIME-GAP: exact record encoding]. */
static void scc_reg_write(u16 index, u32 value)
{
	switch (scc_width) {
	case 4: outl(value, scc_port); break;
	case 2: outw(value & 0xffff, scc_port); break;
	default: outb(value & 0xff, scc_port); break;
	}
}

/* ------------------------------------------------------------------ *
 * Camera power-on (front OV2680, SSCrdG2TiSensor::SensorPowerOn)     *
 * ------------------------------------------------------------------ *
 * Sequence from SCC_register_map.md §4, in order:
 *   1. SSTps68470VoltageWF::Initialize -> 5 rails: 0x41,0x40,0x42,0x3c,0x3f
 *   2. SetVACtl     reg 0x47  -> bit 0 = enable
 *   3. SetVCMCtl    reg 0x44
 *   4. IoActive     reg 0x43
 *   + power-rail enables: reg 0x1a = 0x8a, reg 0x1c = 0x8a  (SSTps68470VoltageUF)
 *   + MCLK enable:    reg 0x0d bit 0 (|1 on) ; rate reg 0x0f bits 2-3 (config field)
 *
 * The rail enables (0x8a) and the MCLK enable/bit layout are STATICALLY KNOWN.
 * The rail voltages (0x41..0x3f) and MCLK rate are [RUNTIME-GAP].
 *------------------------------------------------------------------ */
static int scc_camera_power_on(void)
{
	u32 tmp;

	/* [KNOWN] power-rail channel enables (SSTps68470VoltageUF::Initialize, 0x1400072f8) */
	scc_reg_write(0x1a, 0x8a);   /* 0x8a = 0b1000_1010, bit 7 = power-channel enable */
	scc_reg_write(0x1c, 0x8a);

	/* [RUNTIME-GAP] 5 voltage rails, config-decoded values (see table above) */
	scc_reg_write(0x41, scc_rail_value[0x41]);
	scc_reg_write(0x40, scc_rail_value[0x40]);
	scc_reg_write(0x42, scc_rail_value[0x42]);
	scc_reg_write(0x3c, scc_rail_value[0x3c]);
	scc_reg_write(0x3f, scc_rail_value[0x3f]);

	/* [KNOWN] analog/CM/IO control */
	tmp = scc_port ? inb(scc_port) : 0;      /* read-modify if the reg is RMW; here set bit0 of 0x47 */
	scc_reg_write(0x47, tmp | 0x1);          /* SetVACtl: bit 0 = enable */
	scc_reg_write(0x44, 0x1);                /* SetVCMCtl */
	scc_reg_write(0x43, 0x1);                /* IoActive */

	/* [KNOWN] MCLK: Tps68470Clock::SetHCLKAB (0x14000a524)
	 *   read 0x0d, clear enable (bit0), set 0x0f bits2-3 (rate), re-enable 0x0d bit0 */
	/* [RUNTIME-GAP] MCLK rate = 2 vs 3 MHz field from board config (param_1[0x16/0x17]) */
	scc_reg_write(0x0f, 0x0);                /* clear rate select (bits 2-3) */
	scc_reg_write(0x0d, 0x1);                /* enable MCLK (bit 0) */
	return 0;
}

static int scc_camera_power_off(void)
{
	/* inverse: disable MCLK, clear controls, drop rails */
	scc_reg_write(0x0d, 0x0);                /* disable MCLK */
	scc_reg_write(0x43, 0x0);
	scc_reg_write(0x44, 0x0);
	scc_reg_write(0x47, 0x0);
	scc_reg_write(0x1a, 0x0);
	scc_reg_write(0x1c, 0x0);
	return 0;
}

static int __init scc_proto_init(void)
{
	int rc = scc_port_open();
	if (rc)
		return rc;
	pr_notice("scc: prototype loaded, port=0x%x width=%u", scc_port, scc_width);
	if (scc_port == 0 || scc_width == 0) {
		pr_warn("scc: [RUNTIME-GAP] unset - call scc_camera_power_on() only after filling the port/rate\n");
		return 0;
	}
	rc = scc_camera_power_on();
	pr_notice("scc: power-on sequence issued (rc=%d) -- VERIFY camera comes up", rc);
	return rc;
}

static void __exit scc_proto_exit(void)
{
	scc_camera_power_off();
	scc_port_close();
	pr_notice("scc: unloaded\n");
}

module_init(scc_proto_init);
module_exit(scc_proto_exit);

MODULE_LICENSE("GPL-2.0-only");
MODULE_AUTHOR("chuwi camera debug (prototype)");
MODULE_DESCRIPTION("Prototype SCC port-emulation camera power-on for Chuwi UBook XPro");
MODULE_VERSION("0.1-prototype");
