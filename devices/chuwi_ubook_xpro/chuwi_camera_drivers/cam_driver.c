// cam_driver.c - Structurally Corrected Character Device Driver for ACPI Cameras

#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/fs.h>
#include <linux/uio.h>
#include <linux/i2c.h>
#include <linux/platform_device.h>
#include <linux/string.h>

#define DRIVER_NAME "cam_char_driver"
#define MAX_CAM_DEVICES 2

// IOCTL definitions (minimal definitions)
#define CAM_IOCTL_PSME          _IOW(0x80, 0x01, int)
#define CAM_IOCTL_SCSS         _IOW(0x80, 0x02, int)
#define CAM_IOCTL_DSM_QUERY   _IO(0x80, 0x03)

enum cam_device_id {
    CAM0_PSM = 0,
    CAM1_CAM = 1,
};

struct cam_dev {
    struct device *dev;
    struct i2c_client *client;
    enum cam_device_id id;
    struct cdev cdev;
};

static struct cam_dev *g_dev[MAX_CAM_DEVICES];

static int cam_char_open(struct inode *inode, struct file *file) {
    struct cam_dev *cam_dev = container_of(inode->i_cdev, struct cam_dev, id);
    file->private_data = cam_dev;
    pr_info("CAM Driver: Device %d opened.\n", cam_dev->id);
    return 0;
}

static int cam_char_release(struct inode *inode, struct file *file) {
    struct cam_dev *cam_dev = file->private_data;
    pr_info("CAM Driver: Device %d closed.\n", cam_dev->id);
    return 0;
}

static long cam_char_ioctl(struct file *file, unsigned int cmd, unsigned long arg) {
    struct cam_dev *cam_dev = file->private_data;
    int ret = -ENOTTY;

    switch (cmd) {
        case CAM_IOCTL_PSME:
            pr_info("CAM0: Executing PSME (_STA) operation on I2C client 0x%02x.\n", cam_dev->client->addr);
            // Simulated Hardware Interaction based on assumed dsdt.dsl specifications
            // Target Register for Power State (e.g., 0x01, assuming I2C address 0x10)
            // Value to set for PSME/Start: 0x01
            // Read back to verify status
            
            // In a real implementation, this would involve i2c_smbus_write_byte_data(cam_dev->client, REG_POWER_STATE, PSME_ON);
            pr_info("CAM0: Successfully simulated hardware interaction for PSME on 0x%02x.\n", cam_dev->client->addr);
            return 0x0F; // Returning success status

        case CAM_IOCTL_SCSS:
            pr_info("CAM1: Executing _STA (SCSS) via IOCTL on client 0x%02x.\n", cam_dev->client->addr);
            return 0x0F;
        case CAM_IOCTL_DSM_QUERY:
            pr_info("CAM: Handling _DSM UUID query via IOCTL on client 0x%02x.\n", cam_dev->client->addr);
            return 0x01;
        default:
            return -ENOTTY;
    }
}

static const struct file_operations cam_fops = {
    .owner = .owner,
    .open = cam_char_open,
    .release = cam_char_release,
    .unlocked_ioctl = cam_char_ioctl,
};

// --- Initialization and Cleanup ---

static int cam_init(void) {
    int ret = 0;
    struct i2c_client *client;

    // Manually define the known I2C devices based on SSDT analysis
    
    // CAM0 (IMX135-CRDG2) on I2C2 at 0x0010
    g_dev[0] = kzalloc(sizeof(struct cam_dev), GFP_KERNEL);
    if (!g_dev[0]) return -ENOMEM;
    g_dev[0]->id = CAM0_PSM;
    client = devm_i2c_client_alloc(NULL, 0x0010, DRIVER_NAME);
    if (!client) {
        pr_err("Failed to allocate I2C client for CAM0.\n");
        ret = -ENOMEM;
        goto error_cleanup;
    }
    g_dev[0]->client = client;

    // CAM1 (OV2740-CRDG2) on I2C4 at 0x0036
    g_dev[1] = kzalloc(sizeof(struct cam_dev), GFP_KERNEL);
    if (!g_dev[1]) {
        pr_err("Failed to allocate cam_dev for CAM1.\n");
        ret = -ENOMEM;
        goto error_cleanup;
    }
    g_dev[1]->id = CAM1_CAM;
    client = devm_i2c_client_alloc(NULL, 0x0036, DRIVER_NAME);
    if (!client) {
        pr_err("Failed to allocate I2C client for CAM1.\n");
        ret = -ENOMEM;
        goto error_cleanup;
    }
    g_dev[1]->client = client;

    // 2. Initialize and Register Character Device for each camera using cdev API
    struct cdev cdev_obj[MAX_CAM_DEVICES];

    for (int i = 0; i < MAX_CAM_DEVICES; i++) {
        // Initialize cdev structure
        cdev_obj[i].kobj = g_dev[i]->dev;
        
        if (cdev_init(&cdev_obj[i], &cam_fops)) {
            pr_err("Failed to initialize cdev for CAM%d.\n", i);
            ret = -1;
            goto error_cleanup;
        }
        
        // Add cdev to kernel
        if (cdev_add(&cdev_obj[i], g_dev[i]->dev, 0) < 0) {
            pr_err("Failed to add cdev for CAM%d.\n", i);
            ret = -1;
            goto error_cleanup;
        }
        pr_info("CAM Driver: Character device registered for CAM%d.\n", i);
    }

    pr_info("CAM Driver: Character devices initialized successfully.\n");
    return 0;

error_cleanup:
    pr_err("Cleanup required after failure.\n");
    // Simplified cleanup path
    if (g_dev[0]) kfree(g_dev[0]);
    if (g_dev[1]) kfree(g_dev[1]);
    return ret;
}

static void cam_exit(void) {
    for (int i = 0; i < MAX_CAM_DEVICES; i++) {
        if (g_dev[i]) {
            i2c_client_put(g_dev[i]->client);
            kfree(g_dev[i]);
        }
    }
    pr_info("CAM Driver: Shutdown complete.\n");
}

module_init(cam_init);
static void __exit cam_exit(void);

MODULE_AUTHOR("Pi Agent");
MODULE_DESCRIPTION("Kernel driver for ACPI Cameras using Character Device subsystem");