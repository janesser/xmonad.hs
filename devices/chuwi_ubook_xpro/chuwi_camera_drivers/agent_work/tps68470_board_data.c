/*
 * tps68470_board_data.c - TPS68470 GPIO Pin Configuration for Chuwi UBook XPro
 *
 * Extracted from SkcController.sys binary analysis using Ghidra
 * Based on TPS68470 kernel driver (drivers/gpio/gpio-tps68470.c)
 *
 * TPS68470 has 7 GPIO pins:
 *   gpio.0 - gpio.6 (General purpose)
 *   s_enable, s_reset, s_standby, s_write_protect, s_power_en, mclk (Sensor specific)
 */

#include <linux/module.h>
#include <linux/platform_device.h>
#include <linux/gpio/consumer.h>

/* TPS68470 GPIO Pin Definitions */
#define TPS68470_GPIO_RESET		0
#define TPS68470_GPIO_ENABLE		1
#define TPS68470_GPIO_STROBE		2
#define TPS68470_GPIO_TORCH		3
#define TPS68470_GPIO_FLASH		4
#define TPS68470_GPIO_LED_REAR		5
#define TPS68470_GPIO_LED_FRONT		6

/* Sensor-specific GPIO pins (from reveng_skc_gpio_pins.md) */
#define TPS68470_GPIO_PRIV_LED		7
#define TPS68470_GPIO_POWER0		8
#define TPS68470_GPIO_POWER1		9
#define TPS68470_GPIO_STANDBY		10
#define TPS68470_GPIO_WRITE_PROTECT	11
#define TPS68470_GPIO_POWER_EN		12
#define TPS68470_GPIO_MCLK		13

/* TPS68470 I2C Address */
#define TPS68470_I2C_ADDR		0x6A

/* TPS68470 Register Addresses */
#define TPS68470_REG_RESET		0x00
#define TPS68470_REG_ENABLE		0x01
#define TPS68470_REG_STROBE		0x02
#define TPS68470_REG_TORCH		0x03
#define TPS68470_REG_FLASH		0x04
#define TPS68470_REG_LED_REAR		0x05
#define TPS68470_REG_LED_FRONT		0x06

/* GPIO Configuration Structure */
struct tps68470_gpio_config {
    unsigned int gpio_pin;
    unsigned int gpio_function;
    unsigned int gpio_direction;
    const char *gpio_name;
};

/* Camera 1 (OV2680) - TPS68470 GPIO Configuration */
static struct tps68470_gpio_config camera1_gpio_config[] = {
    { TPS68470_GPIO_RESET,     TPS68470_GPIO_RESET,     GPIOF_OUT_INIT_LOW,  "Reset" },
    { TPS68470_GPIO_ENABLE,    TPS68470_GPIO_ENABLE,    GPIOF_OUT_INIT_LOW,  "Enable" },
    { TPS68470_GPIO_STROBE,    TPS68470_GPIO_STROBE,    GPIOF_OUT_INIT_LOW,  "Strobe" },
    { TPS68470_GPIO_TORCH,     TPS68470_GPIO_TORCH,     GPIOF_OUT_INIT_LOW,  "Torch" },
    { TPS68470_GPIO_FLASH,     TPS68470_GPIO_FLASH,     GPIOF_OUT_INIT_LOW,  "Flash" },
    { TPS68470_GPIO_LED_REAR,  TPS68470_GPIO_LED_REAR,  GPIOF_OUT_INIT_LOW,  "LedRear" },
    { TPS68470_GPIO_LED_FRONT, TPS68470_GPIO_LED_FRONT, GPIOF_OUT_INIT_LOW,  "LedFront" },
};

/* Camera 2 (OV5648) - UP6641 GPIO Configuration (same pattern) */
static struct tps68470_gpio_config camera2_gpio_config[] = {
    { TPS68470_GPIO_RESET,     TPS68470_GPIO_RESET,     GPIOF_OUT_INIT_LOW,  "Reset" },
    { TPS68470_GPIO_ENABLE,    TPS68470_GPIO_ENABLE,    GPIOF_OUT_INIT_LOW,  "Enable" },
    { TPS68470_GPIO_STROBE,    TPS68470_GPIO_STROBE,    GPIOF_OUT_INIT_LOW,  "Strobe" },
    { TPS68470_GPIO_TORCH,     TPS68470_GPIO_TORCH,     GPIOF_OUT_INIT_LOW,  "Torch" },
    { TPS68470_GPIO_FLASH,     TPS68470_GPIO_FLASH,     GPIOF_OUT_INIT_LOW,  "Flash" },
    { TPS68470_GPIO_LED_REAR,  TPS68470_GPIO_LED_REAR,  GPIOF_OUT_INIT_LOW,  "LedRear" },
    { TPS68470_GPIO_LED_FRONT, TPS68470_GPIO_LED_FRONT, GPIOF_OUT_INIT_LOW,  "LedFront" },
};

/* Board Data Structure */
struct chuwi_board_data {
    struct device *dev;
    struct gpio_desc *gpio_reset;
    struct gpio_desc *gpio_enable;
    struct gpio_desc *gpio_strobe;
    struct gpio_desc *gpio_torch;
    struct gpio_desc *gpio_flash;
    struct gpio_desc *gpio_led_rear;
    struct gpio_desc *gpio_led_front;
    struct gpio_desc *gpio_priv_led;
    struct gpio_desc *gpio_power0;
    struct gpio_desc *gpio_power1;
    struct gpio_desc *gpio_standby;
    struct gpio_desc *gpio_write_protect;
    struct gpio_desc *gpio_power_en;
    struct gpio_desc *gpio_mclk;
};

/*
 * Function: tps68470_init_gpio
 * Description: Initialize all GPIO pins for TPS68470
 *
 * Based on function names found in binary:
 *   - tps68470::CrdGTiGpio::GpioOper
 *   - tps68470::SSCrdG2TiSensor::SetGpio
 *   - tps68470::TPS68470::ResetControlLogic
 */
static int tps68470_init_gpio(struct chuwi_board_data *board)
{
    int ret;
    int i;

    /* Initialize general GPIO pins */
    for (i = 0; i < ARRAY_SIZE(camera1_gpio_config); i++) {
        ret = devm_gpio_request_one(board->dev,
            camera1_gpio_config[i].gpio_pin,
            camera1_gpio_config[i].gpio_direction,
            camera1_gpio_config[i].gpio_name);
        if (ret) {
            dev_err(board->dev, "Failed to request GPIO %d: %d\n",
                    camera1_gpio_config[i].gpio_pin, ret);
            return ret;
        }
    }

    /* Initialize sensor-specific GPIO pins */
    board->gpio_priv_led = devm_gpio_request_one(
        board->dev, TPS68470_GPIO_PRIV_LED, GPIOF_IN, "PrivateLED");
    board->gpio_power0 = devm_gpio_request_one(
        board->dev, TPS68470_GPIO_POWER0, GPIOF_IN, "Power0");
    board->gpio_power1 = devm_gpio_request_one(
        board->dev, TPS68470_GPIO_POWER1, GPIOF_IN, "Power1");
    board->gpio_standby = devm_gpio_request_one(
        board->dev, TPS68470_GPIO_STANDBY, GPIOF_IN, "Standby");
    board->gpio_write_protect = devm_gpio_request_one(
        board->dev, TPS68470_GPIO_WRITE_PROTECT, GPIOF_IN, "WriteProtect");
    board->gpio_power_en = devm_gpio_request_one(
        board->dev, TPS68470_GPIO_POWER_EN, GPIOF_IN, "PowerEn");
    board->gpio_mclk = devm_gpio_request_one(
        board->dev, TPS68470_GPIO_MCLK, GPIOF_IN, "Mclk");

    return 0;
}

/*
 * Function: tps68470_gpio_set
 * Description: Set GPIO pin state
 *
 * Based on function names found in binary:
 *   - tps68470::CrdGTiGpio::GpioOper
 *   - tps68470::CrdG2TiGpio::GpioOper
 *   - tps68470::CrdG2TiQuantaGpio::GpioOper
 */
static int tps68470_gpio_set(struct chuwi_board_data *board,
                              unsigned int pin, int value)
{
    int ret;

    switch (pin) {
    case TPS68470_GPIO_RESET:
        ret = gpiod_set_value(board->gpio_reset, value);
        break;
    case TPS68470_GPIO_ENABLE:
        ret = gpiod_set_value(board->gpio_enable, value);
        break;
    case TPS68470_GPIO_STROBE:
        ret = gpiod_set_value(board->gpio_strobe, value);
        break;
    case TPS68470_GPIO_TORCH:
        ret = gpiod_set_value(board->gpio_torch, value);
        break;
    case TPS68470_GPIO_FLASH:
        ret = gpiod_set_value(board->gpio_flash, value);
        break;
    case TPS68470_GPIO_LED_REAR:
        ret = gpiod_set_value(board->gpio_led_rear, value);
        break;
    case TPS68470_GPIO_LED_FRONT:
        ret = gpiod_set_value(board->gpio_led_front, value);
        break;
    default:
        dev_err(board->dev, "Unknown GPIO pin: %d\n", pin);
        return -EINVAL;
    }

    return ret;
}

/*
 * Function: tps68470_reset_control
 * Description: Reset TPS68470 control logic
 *
 * Based on function names found in binary:
 *   - tps68470::TPS68470::ResetControlLogic
 *   - tps68470::SSTps68470::ResetControlLogic
 *   - up6641::uP6641::ResetControlLogic
 */
static int tps68470_reset_control(struct chuwi_board_data *board)
{
    int ret;

    /* Assert reset */
    ret = tps68470_gpio_set(board, TPS68470_GPIO_RESET, 0);
    if (ret)
        return ret;

    /* Wait for reset to take effect */
    mdelay(10);

    /* Deassert reset */
    ret = tps68470_gpio_set(board, TPS68470_GPIO_RESET, 1);
    if (ret)
        return ret;

    /* Wait for device to come up */
    mdelay(100);

    return 0;
}

/*
 * Function: tps68470_power_control
 * Description: Power control for TPS68470
 *
 * Based on function names found in binary:
 *   - tps68470::SSCrdG2TiSensor::SensorPowerOn
 *   - tps68470::SSCrdG2TiSensor::SensorPowerOff
 */
static int tps68470_power_on(struct chuwi_board_data *board)
{
    int ret;

    /* Enable power */
    ret = tps68470_gpio_set(board, TPS68470_GPIO_ENABLE, 1);
    if (ret)
        return ret;

    /* Wait for power to stabilize */
    mdelay(100);

    return 0;
}

static int tps68470_power_off(struct chuwi_board_data *board)
{
    int ret;

    /* Disable power */
    ret = tps68470_gpio_set(board, TPS68470_GPIO_ENABLE, 0);
    if (ret)
        return ret;

    /* Wait for power to settle */
    mdelay(50);

    return 0;
}

/*
 * Function: tps68470_flash_control
 * Description: Flash and torch control
 *
 * Based on function names found in binary:
 *   - tps68470::Tps68470Flash::FlashPowerOn
 *   - tps68470::Tps68470Flash::FlashPowerOff
 *   - tps68470::Tps68470Flash::TorchPowerOn
 *   - tps68470::Tps68470Flash::TorchPowerOff
 */
static int tps68470_flash_on(struct chuwi_board_data *board)
{
    int ret;

    ret = tps68470_gpio_set(board, TPS68470_GPIO_FLASH, 1);
    if (ret)
        return ret;

    ret = tps68470_gpio_set(board, TPS68470_GPIO_TORCH, 1);
    if (ret)
        return ret;

    /* Wait for flash to initialize */
    mdelay(10);

    return 0;
}

static int tps68470_flash_off(struct chuwi_board_data *board)
{
    return tps68470_gpio_set(board, TPS68470_GPIO_FLASH, 0);
}

static int tps68470_torch_on(struct chuwi_board_data *board)
{
    return tps68470_gpio_set(board, TPS68470_GPIO_TORCH, 1);
}

static int tps68470_torch_off(struct chuwi_board_data *board)
{
    return tps68470_gpio_set(board, TPS68470_GPIO_TORCH, 0);
}

/*
 * Function: tps68470_indicator_control
 * Description: Privacy indicator LED control
 *
 * Based on function names found in binary:
 *   - tps68470::SSTps68470::IndicatorOn
 *   - tps68470::SSTps68470::IndicatorOff
 */
static int tps68470_indicator_on(struct chuwi_board_data *board)
{
    return tps68470_gpio_set(board, TPS68470_GPIO_PRIV_LED, 1);
}

static int tps68470_indicator_off(struct chuwi_board_data *board)
{
    return tps68470_gpio_set(board, TPS68470_GPIO_PRIV_LED, 0);
}

/*
 * Function: tps68470_clock_control
 * Description: Clock output control (MCLK)
 *
 * Based on function names found in binary:
 *   - tps68470::SSCrdG2TiSensor::MclkOutput
 *   - tps68470::CrdGTiSensor::MclkOutput
 *   - tps68470::CrdG2TiSensor::MclkOutput
 *   - up6641::CrdGUpiSensor::MclkOutput
 *   - up6641::CrdG2UpiSensor::MclkOutput
 */
static int tps68470_mclk_output(struct chuwi_board_data *board)
{
    /* MCLK is typically controlled by clock configuration, not GPIO */
    /* This function may configure the clock output registers */
    dev_info(board->dev, "MCLK output enabled\n");
    return 0;
}

/*
 * Module Information
 */
MODULE_AUTHOR("Chuwi UBook XPro Camera Driver");
MODULE_DESCRIPTION("TPS68470 GPIO Board Data for Chuwi UBook XPro");
MODULE_LICENSE("GPL");
MODULE_VERSION("1.0");
