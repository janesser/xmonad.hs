#include <linux/module.h>
#define INCLUDE_VERMAGIC
#include <linux/build-salt.h>
#include <linux/elfnote-lto.h>
#include <linux/export-internal.h>
#include <linux/vermagic.h>
#include <linux/compiler.h>

#ifdef CONFIG_UNWINDER_ORC
#include <asm/orc_header.h>
ORC_HEADER;
#endif

BUILD_SALT;
BUILD_LTO_INFO;

MODULE_INFO(vermagic, VERMAGIC_STRING);
MODULE_INFO(name, KBUILD_MODNAME);

__visible struct module __this_module
__section(".gnu.linkonce.this_module") = {
	.name = KBUILD_MODNAME,
	.init = init_module,
#ifdef CONFIG_MODULE_UNLOAD
	.exit = cleanup_module,
#endif
	.arch = MODULE_ARCH_INIT,
};

#ifdef CONFIG_MITIGATION_RETPOLINE
MODULE_INFO(retpoline, "Y");
#endif



static const char ____versions[]
__used __section("__versions") =
	"\x1c\x00\x00\x00\xf7\x61\x74\x6e"
	"is_acpi_device_node\0"
	"\x18\x00\x00\x00\x28\xa1\xe8\x31"
	"regmap_write\0\0\0\0"
	"\x18\x00\x00\x00\x19\x53\xe1\x51"
	"devm_kmalloc\0\0\0\0"
	"\x10\x00\x00\x00\xba\x0c\x7a\x03"
	"kfree\0\0\0"
	"\x14\x00\x00\x00\xbb\x6d\xfb\xbd"
	"__fentry__\0\0"
	"\x20\x00\x00\x00\x4a\xed\x1e\x05"
	"__devm_regmap_init_i2c\0\0"
	"\x1c\x00\x00\x00\xcb\xf6\xfd\xf0"
	"__stack_chk_fail\0\0\0\0"
	"\x14\x00\x00\x00\xd7\x52\xf8\xc7"
	"put_device\0\0"
	"\x14\x00\x00\x00\x90\xf6\x65\xb1"
	"_dev_info\0\0\0"
	"\x1c\x00\x00\x00\x7d\x7d\xec\x87"
	"i2c_register_driver\0"
	"\x14\x00\x00\x00\x45\xec\x71\x76"
	"_dev_err\0\0\0\0"
	"\x1c\x00\x00\x00\x63\xa5\x03\x4c"
	"random_kmalloc_seed\0"
	"\x1c\x00\x00\x00\xac\x08\xdf\xd8"
	"acpi_handle_printk\0\0"
	"\x1c\x00\x00\x00\xca\x39\x82\x5b"
	"__x86_return_thunk\0\0"
	"\x10\x00\x00\x00\x5a\x25\xd5\xe2"
	"strcmp\0\0"
	"\x18\x00\x00\x00\x00\x72\xc9\x56"
	"devm_kasprintf\0\0"
	"\x20\x00\x00\x00\x97\x13\xaa\x38"
	"gpiod_add_lookup_table\0\0"
	"\x14\x00\x00\x00\x9a\xe5\x94\x87"
	"regmap_read\0"
	"\x20\x00\x00\x00\x6b\xeb\x2d\xd9"
	"acpi_evaluate_object\0\0\0\0"
	"\x24\x00\x00\x00\x7d\x48\x81\xff"
	"gpiod_remove_lookup_table\0\0\0"
	"\x18\x00\x00\x00\x13\x87\x45\x49"
	"dev_err_probe\0\0\0"
	"\x18\x00\x00\x00\x2d\xa2\xcb\x94"
	"kmalloc_trace\0\0\0"
	"\x18\x00\x00\x00\x33\xb6\x70\x13"
	"i2c_del_driver\0\0"
	"\x28\x00\x00\x00\x09\x29\x8b\x74"
	"acpi_dev_get_next_consumer_dev\0\0"
	"\x20\x00\x00\x00\x3e\xae\x80\x00"
	"devm_mfd_add_devices\0\0\0\0"
	"\x18\x00\x00\x00\xb9\x77\x08\x05"
	"dmi_first_match\0"
	"\x18\x00\x00\x00\xdb\xf2\xc9\xf5"
	"kmalloc_caches\0\0"
	"\x18\x00\x00\x00\x13\xc9\x9a\x0d"
	"module_layout\0\0\0"
	"\x00\x00\x00\x00\x00\x00\x00\x00";

MODULE_INFO(depends, "");

MODULE_ALIAS("acpi*:INT3472:*");

MODULE_INFO(srcversion, "74B7482298F1AEB1FEE8949");
