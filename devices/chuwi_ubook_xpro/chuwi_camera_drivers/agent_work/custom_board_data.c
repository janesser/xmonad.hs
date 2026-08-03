// Custom board data for Chuwi UBook XPro with Surface Go GPIO pins as starting point
// This file adds Chuwi DMI info to the INT3472/TPS68470 board data table

#include <linux/dmi.h>
#include <linux/gpio/machine.h>
#include <linux/platform_data/tps68470.h>
#include <linux/regulator/machine.h>
#include "tps68470.h"

// Same regulator configuration as Surface Go
static struct regulator_consumer_supply chuwi_core_consumer_supplies[] = {
	REGULATOR_SUPPLY("dvdd", "i2c-INT347A:00"),
};

static struct regulator_consumer_supply chuwi_ana_consumer_supplies[] = {
	REGULATOR_SUPPLY("avdd", "i2c-INT347A:00"),
};

static struct regulator_consumer_supply chuwi_vcm_consumer_supplies[] = {
	REGULATOR_SUPPLY("vdd", "i2c-INT347A:00-VCM"),
};

static struct regulator_consumer_supply chuwi_vsio_consumer_supplies[] = {
	REGULATOR_SUPPLY("dovdd", "i2c-INT347A:00"),
	REGULATOR_SUPPLY("vsio", "i2c-INT347A:00-VCM"),
	REGULATOR_SUPPLY("vddd", "i2c-INT347E:00"),
};

static struct regulator_consumer_supply chuwi_aux1_consumer_supplies[] = {
	REGULATOR_SUPPLY("vdda", "i2c-INT347E:00"),
};

static struct regulator_consumer_supply chuwi_aux2_consumer_supplies[] = {
	REGULATOR_SUPPLY("vdddo", "i2c-INT347E:00"),
};

static const struct regulator_init_data chuwi_core_reg_init_data = {
	.constraints = {
		.min_uV = 1200000,
		.max_uV = 1200000,
		.apply_uV = true,
		.valid_ops_mask = REGULATOR_CHANGE_STATUS,
	},
	.num_consumer_supplies = ARRAY_SIZE(chuwi_core_consumer_supplies),
	.consumer_supplies = chuwi_core_consumer_supplies,
};

static const struct regulator_init_data chuwi_ana_reg_init_data = {
	.constraints = {
		.min_uV = 2815200,
		.max_uV = 2815200,
		.apply_uV = true,
		.valid_ops_mask = REGULATOR_CHANGE_STATUS,
	},
	.num_consumer_supplies = ARRAY_SIZE(chuwi_ana_consumer_supplies),
	.consumer_supplies = chuwi_ana_consumer_supplies,
};

static const struct regulator_init_data chuwi_vcm_reg_init_data = {
	.constraints = {
		.min_uV = 2815200,
		.max_uV = 2815200,
		.apply_uV = true,
		.valid_ops_mask = REGULATOR_CHANGE_STATUS,
	},
	.num_consumer_supplies = ARRAY_SIZE(chuwi_vcm_consumer_supplies),
	.consumer_supplies = chuwi_vcm_consumer_supplies,
};

static const struct regulator_init_data chuwi_vio_reg_init_data = {
	.constraints = {
		.min_uV = 1800600,
		.max_uV = 1800600,
		.apply_uV = true,
		.always_on = true,
	},
};

static const struct regulator_init_data chuwi_vsio_reg_init_data = {
	.constraints = {
		.min_uV = 1800600,
		.max_uV = 1800600,
		.apply_uV = true,
		.valid_ops_mask = REGULATOR_CHANGE_STATUS,
	},
	.num_consumer_supplies = ARRAY_SIZE(chuwi_vsio_consumer_supplies),
	.consumer_supplies = chuwi_vsio_consumer_supplies,
};

static const struct regulator_init_data chuwi_aux1_reg_init_data = {
	.constraints = {
		.min_uV = 2815200,
		.max_uV = 2815200,
		.apply_uV = 1,
		.valid_ops_mask = REGULATOR_CHANGE_STATUS,
	},
	.num_consumer_supplies = ARRAY_SIZE(chuwi_aux1_consumer_supplies),
	.consumer_supplies = chuwi_aux1_consumer_supplies,
};

static const struct regulator_init_data chuwi_aux2_reg_init_data = {
	.constraints = {
		.min_uV = 1800600,
		.max_uV = 1800600,
		.apply_uV = 1,
		.valid_ops_mask = REGULATOR_CHANGE_STATUS,
	},
	.num_consumer_supplies = ARRAY_SIZE(chuwi_aux2_consumer_supplies),
	.consumer_supplies = chuwi_aux2_consumer_supplies,
};

static const struct tps68470_regulator_platform_data chuwi_tps68470_pdata = {
	.reg_init_data = {
		[TPS68470_CORE] = &chuwi_core_reg_init_data,
		[TPS68470_ANA]  = &chuwi_ana_reg_init_data,
		[TPS68470_VCM]  = &chuwi_vcm_reg_init_data,
		[TPS68470_VIO] = &chuwi_vio_reg_init_data,
		[TPS68470_VSIO] = &chuwi_vsio_reg_init_data,
		[TPS68470_AUX1] = &chuwi_aux1_reg_init_data,
		[TPS68470_AUX2] = &chuwi_aux2_reg_init_data,
	},
};

// GPIO pins: Using Surface Go pins (9, 7, 5) as starting point
// INT347A:00 (CAM0): GPIO 9 (reset, active-low), GPIO 7 (powerdown, active-low)
// INT347E:00 (CAM1): GPIO 5 (enable, active-high)
static struct gpiod_lookup_table chuwi_int347a_gpios = {
	.dev_id = "i2c-INT347A:00",
	.table = {
		GPIO_LOOKUP("tps68470-gpio", 9, "reset", GPIO_ACTIVE_LOW),
		GPIO_LOOKUP("tps68470-gpio", 7, "powerdown", GPIO_ACTIVE_LOW),
		{ }
	}
};

static struct gpiod_lookup_table chuwi_int347e_gpios = {
	.dev_id = "i2c-INT347E:00",
	.table = {
		GPIO_LOOKUP("tps68470-gpio", 5, "enable", GPIO_ACTIVE_HIGH),
		{ }
	}
};

static const struct int3472_tps68470_board_data chuwi_tps68470_board_data = {
	.dev_name = "i2c-INT3472:00",
	.tps68470_regulator_pdata = &chuwi_tps68470_pdata,
	.n_gpiod_lookups = 2,
	.tps68470_gpio_lookup_tables = {
		&chuwi_int347a_gpios,
		&chuwi_int347e_gpios,
	},
};

static const struct dmi_system_id chuwi_tps68470_board_data_table[] = {
	{
		.matches = {
			DMI_EXACT_MATCH(DMI_SYS_VENDOR, "CHUWI Innovation And Technology(ShenZhen)co.,Ltd"),
			DMI_EXACT_MATCH(DMI_PRODUCT_NAME, "UBook XPro"),
		},
		.driver_data = (void *)&chuwi_tps68470_board_data,
	},
	{ }
};

// Add this to the existing board data table in tps68470_board_data.c
// Before the final { } entry
static const struct dmi_system_id *chuwi_tps68470_get_board_data(const char *dev_name)
{
	const struct int3472_tps68470_board_data *board_data;
	const struct dmi_system_id *match;

	for (match = dmi_first_match(chuwi_tps68470_board_data_table);
	     match;
	     match = dmi_first_match(match + 1)) {
		board_data = match->driver_data;
		if (strcmp(board_data->dev_name, dev_name) == 0)
			return board_data;
	}

	return NULL;
}
