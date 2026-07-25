/*
 * cam1_driver.c - CAM1 (OV2740) camera driver for Chuwi UBook XPro
 *
 * Based on DSDT ACPI specification from dsdt.cam0_cam1.dsl
 *
 * DSDT defines:
 *   Device (CAM1)
 *     Name (_ADR, Zero)  // Address 0x00
 *     Name (_HID, "INT3474")  // Hardware ID
 *     Name (_CID, "INT3474")  // Compatible ID
 *     Name (_DDN, "OV2740-CRDG2")  // DOS Device Name
 *     I2C Address: 0x36
 *     I2C Bus: _SB.PCI0.I2C4
 *     Dependencies: PMIC
 *
 * Kernel: 7.0.0-28-generic
 */

#include <linux/module.h>
#include <linux/i2c.h>
#include <linux/media/device.h>
#include <linux/v4l2_subdev.h>
#include <linux/v4l2-device.h>
#include <linux/v4l2-ioctl.h>
#include <linux/slab.h>

static struct i2c_client *cam1_i2c_client = NULL;
static struct v4l2_subdev *cam1_subdev = NULL;

static int cam1_i2c_probe(struct i2c_client *client, const struct i2c_device_id *id)
{
    struct v4l2_subdev_state *sd_state;

    pr_info("CAM1: CAM1 driver probe\n");
    pr_info("CAM1: I2C client: bus=%d, addr=%02x\n",
            client->addr, client->addr);

    cam1_i2c_client = client;

    sd_state = v4l2_subdev_open(&client->dev, V4L2_SUBDEV_STATE_OPEN);
    if (!sd_state) {
        pr_err("CAM1: Failed to open I2C client\n");
        return -ENODEV;
    }

    cam1_subdev = v4l2_subdev_new(&client->dev, NULL, cam1_subdev);
    if (!cam1_subdev) {
        pr_err("CAM1: Failed to create v4l2_subdev\n");
        v4l2_subdev_release_handle(&client->dev, sd_state);
        return -ENOMEM;
    }

    pr_info("CAM1: Successfully registered CAM1\n");
    return 0;
}

static int cam1_i2c_remove(struct i2c_client *client)
{
    pr_info("CAM1: CAM1 driver remove\n");
    return 0;
}

static const struct i2c_device_id cam1_device_id[] = {
    { "cam1", 0 },
    { }
};
MODULE_DEVICE_TABLE(i2c, cam1_device_id);

static struct i2c_driver cam1_i2c_driver = {
    .driver = {
        .name = "cam1",
        .probe_new = cam1_i2c_probe,
        .remove = cam1_i2c_remove,
    },
    .id_table = cam1_device_id,
};

static int __init cam1_init(void)
{
    pr_info("CAM1: CAM1 driver initializing\n");
    return i2c_add_driver(&cam1_i2c_driver);
}

static void __exit cam1_exit(void)
{
    i2c_del_driver(&cam1_i2c_driver);
    pr_info("CAM1: CAM1 driver exiting\n");
}

module_init(cam1_init);
module_exit(cam1_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Chuwi UBook XPro Camera Driver");
MODULE_DESCRIPTION("CAM1 (OV2740) Camera Driver");
MODULE_VERSION("1.0");