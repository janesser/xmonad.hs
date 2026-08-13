#!/bin/bash
# Try the exact GPIO pin configuration from the kernel source
# This should be the correct pin mapping for the Chuwi UBook XPro

echo "=== Trying exact GPIO pin configuration from kernel source ==="
echo ""
echo "Chuwi UBook XPro board data specifies:"
echo "  INT347A GPIO 3: reset (active low)"
echo "  INT347A GPIO 4: powerdown (active low)"
echo "  INT347E GPIO 5: enable (active high)"
echo ""
echo "These are TPS68470 GPIO pins, not INT3472 GPIO pins"
echo "Target: Device at 0x44 on i2c-3 (SMBus I801 adapter)"
echo ""

# Function to try a GPIO pin configuration
try_gpio_config() {
    local gpiochip="$1"
    local pins="$2"
    local desc="$3"
    
    echo "Trying: $desc"
    echo "  GPIO chip: $gpiochip"
    echo "  Pins: $pins"
    
    # Export the GPIO pins
    for pin in $(echo $pins | tr ',' ' '); do
        echo "  Exporting gpiochip${gpiochip} gpio ${pin}..."
        echo "gpiochip${gpiochip} gpio ${pin}" >> /sys/class/gpio/export 2>/dev/null
    done
    
    # Wait a moment for GPIO to be exported
    sleep 0.2
    
    # Try different directions and values
    # Reset (active low) - try pulling low
    echo "  Setting reset GPIO low..."
    for pin in $(echo $pins | tr ',' ' '); do
        echo $pin > /sys/class/gpio/gpiochip${gpiochip}/direction 2>/dev/null
        echo 0 > /sys/class/gpio/gpiochip${gpiochip}/gpio${pin}/value 2>/dev/null
        sleep 0.05
    done
    
    # Wait for reset to take effect
    sleep 0.2
    
    # Powerdown (active low) - try pulling high (release)
    echo "  Setting powerdown GPIO high..."
    for pin in $(echo $pins | tr ',' ' '); do
        echo $pin > /sys/class/gpio/gpiochip${gpiochip}/direction 2>/dev/null
        echo 1 > /sys/class/gpio/gpiochip${gpiochip}/gpio${pin}/value 2>/dev/null
        sleep 0.05
    done
    
    # Wait for powerdown to take effect
    sleep 0.2
    
    # Enable (active high) - try pulling high
    echo "  Setting enable GPIO high..."
    for pin in $(echo $pins | tr ',' ' '); do
        echo $pin > /sys/class/gpio/gpiochip${gpiochip}/direction 2>/dev/null
        echo 1 > /sys/class/gpio/gpiochip${gpiochip}/gpio${pin}/value 2>/dev/null
        sleep 0.05
    done
    
    # Wait for enable to take effect
    sleep 0.2
    
    # Check if the PMIC responds
    echo "  Checking if PMIC responds..."
    # Try to read the device name
    device_name=$(cat /sys/bus/i2c/devices/i2c-3/new_device 2>/dev/null)
    if [ -n "$device_name" ]; then
        echo "  SUCCESS! Device name: $device_name"
        echo "  PMIC is responding!"
        return 0
    else
        echo "  No device name found"
    fi
    
    # Unexport the GPIO pins
    echo "  Unexporting GPIO pins..."
    for pin in $(echo $pins | tr ',' ' '); do
        echo "unexport" > /sys/class/gpio/gpiochip${gpiochip}/gpio${pin} 2>/dev/null
    done
    
    return 1
}

# Try the exact configuration from the kernel source
# These are TPS68470 GPIO pins, so they should be on the GPIO controller that exposes TPS68470 GPIOs
echo "=== Trying exact configuration: reset=3, pdwn=4, enable=5 ==="
try_gpio_config 512 "3,4,5" "Exact configuration from kernel source" || true

echo ""
echo "=== Trying nearby pins (0-7) ==="
# Try all combinations of pins 0-7 on GPIO chip 512
for reset_pin in 0 1 2 3 4 5 6 7; do
    for pdwn_pin in 0 1 2 3 4 5 6 7; do
        for enable_pin in 0 1 2 3 4 5 6 7; do
            try_gpio_config 512 "$reset_pin,$pdwn_pin,$enable_pin" "Pins $reset_pin,$pdwn_pin,$enable_pin" || true
        done
    done
done

echo ""
echo "=== Summary ==="
echo "Tried exact configuration and all combinations of pins 0-7"
echo "If no PMIC response was detected, the GPIO pins might be on a different GPIO controller"
echo "or the pin numbers might be different from what's in the kernel source"
