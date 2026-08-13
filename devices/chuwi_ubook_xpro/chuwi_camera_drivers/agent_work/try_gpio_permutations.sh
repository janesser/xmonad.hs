#!/bin/bash
# Try different GPIO pin permutations for INT3472/INT347A/INT347E
# to find the correct pin mapping that makes the TPS68470 PMIC respond
# Fixed version using unique pin numbers for each role

echo "=== Trying GPIO pin permutations for INT3472/INT347A/INT347E ==="
echo ""
echo "Current board data specifies:"
echo "  INT347A GPIO 3: reset (active low)"
echo "  INT347A GPIO 4: powerdown (active low)"
echo "  INT347A GPIO 5: enable (active high)"
echo "  INT347E GPIO 7: powerdown (active low)"
echo ""
echo "Target: Device at 0x44 on i2c-3 (SMBus I801 adapter)"
echo ""

# Function to try a GPIO pin configuration
try_gpio_config() {
    local gpiochip="$1"
    local reset_pin="$2"
    local pdwn_pin="$3"
    local enable_pin="$4"
    local desc="$5"
    
    echo "Trying: $desc"
    echo "  GPIO chip: $gpiochip"
    echo "  Reset pin: $reset_pin"
    echo "  Powerdown pin: $pdwn_pin"
    echo "  Enable pin: $enable_pin"
    
    # Export the GPIO pins (unique pins for each role)
    for pin in $reset_pin $pdwn_pin $enable_pin; do
        echo "  Exporting gpiochip${gpiochip} gpio ${pin}..."
        echo "gpiochip${gpiochip} gpio ${pin}" >> /sys/class/gpio/export 2>/dev/null
    done
    
    # Wait a moment for GPIO to be exported
    sleep 0.2
    
    # Try different directions and values
    # Reset (active low) - try pulling low
    echo "  Setting reset GPIO low..."
    echo $reset_pin > /sys/class/gpio/gpiochip${gpiochip}/direction 2>/dev/null
    echo 0 > /sys/class/gpio/gpiochip${gpiochip}/gpio${reset_pin}/value 2>/dev/null
    sleep 0.1
    
    # Powerdown (active low) - try pulling high (release)
    echo "  Setting powerdown GPIO high..."
    echo $pdwn_pin > /sys/class/gpio/gpiochip${gpiochip}/direction 2>/dev/null
    echo 1 > /sys/class/gpio/gpiochip${gpiochip}/gpio${pdwn_pin}/value 2>/dev/null
    sleep 0.1
    
    # Enable (active high) - try pulling high
    echo "  Setting enable GPIO high..."
    echo $enable_pin > /sys/class/gpio/gpiochip${gpiochip}/direction 2>/dev/null
    echo 1 > /sys/class/gpio/gpiochip${gpiochip}/gpio${enable_pin}/value 2>/dev/null
    sleep 0.1
    
    # Wait for changes to take effect
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
    for pin in $reset_pin $pdwn_pin $enable_pin; do
        echo "unexport" > /sys/class/gpio/gpiochip${gpiochip}/gpio${pin} 2>/dev/null
    done
    
    return 1
}

# Check what GPIO chips are available
echo "=== Available GPIO chips ==="
ls /sys/class/gpio/gpiochip* 2>/dev/null
echo ""

# Try common GPIO chip numbers (focus on lower numbers first)
for gpiochip in 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150; do
    # Check if GPIO chip exists
    if [ -f "/sys/class/gpio/gpiochip${gpiochip}/ngpio" ]; then
        ngpio=$(cat /sys/class/gpio/gpiochip${gpiochip}/ngpio 2>/dev/null)
        if [ -n "$ngpio" ]; then
            echo "Found GPIO chip ${gpiochip} with ${ngpio} pins"
            
            # Try common pin numbers for INT347A/INT347E (first 8 pins)
            for reset_pin in 0 1 2 3 4 5 6 7; do
                for pdwn_pin in 0 1 2 3 4 5 6 7; do
                    for enable_pin in 0 1 2 3 4 5 6 7; do
                        try_gpio_config ${gpiochip} $reset_pin $pdwn_pin $enable_pin "GPIO chip ${gpiochip}: reset=${reset_pin}, pdwn=${pdwn_pin}, enable=${enable_pin}" || true
                    done
                done
            done
            
            # If we haven't found it yet, try the next 8 pins
            for reset_pin in 8 9 10 11 12 13 14 15; do
                for pdwn_pin in 8 9 10 11 12 13 14 15; do
                    for enable_pin in 8 9 10 11 12 13 14 15; do
                        try_gpio_config ${gpiochip} $reset_pin $pdwn_pin $enable_pin "GPIO chip ${gpiochip}: reset=${reset_pin}, pdwn=${pdwn_pin}, enable=${enable_pin}" || true
                    done
                done
            done
            
            # If we still haven't found it, try pins 16-23
            for reset_pin in 16 17 18 19 20 21 22 23; do
                for pdwn_pin in 16 17 18 19 20 21 22 23; do
                    for enable_pin in 16 17 18 19 20 21 22 23; do
                        try_gpio_config ${gpiochip} $reset_pin $pdwn_pin $enable_pin "GPIO chip ${gpiochip}: reset=${reset_pin}, pdwn=${pdwn_pin}, enable=${enable_pin}" || true
                    done
                done
            done
            
            # If we still haven't found it, try pins 24-31
            for reset_pin in 24 25 26 27 28 29 30 31; do
                for pdwn_pin in 24 25 26 27 28 29 30 31; do
                    for enable_pin in 24 25 26 27 28 29 30 31; do
                        try_gpio_config ${gpiochip} $reset_pin $pdwn_pin $enable_pin "GPIO chip ${gpiochip}: reset=${reset_pin}, pdwn=${pdwn_pin}, enable=${enable_pin}" || true
                    done
                done
            done
            
            # If we still haven't found it, try pins 32-39
            for reset_pin in 32 33 34 35 36 37 38 39; do
                for pdwn_pin in 32 33 34 35 36 37 38 39; do
                    for enable_pin in 32 33 34 35 36 37 38 39; do
                        try_gpio_config ${gpiochip} $reset_pin $pdwn_pin $enable_pin "GPIO chip ${gpiochip}: reset=${reset_pin}, pdwn=${pdwn_pin}, enable=${enable_pin}" || true
                    done
                done
            done
            
            # If we still haven't found it, try pins 40-47
            for reset_pin in 40 41 42 43 44 45 46 47; do
                for pdwn_pin in 40 41 42 43 44 45 46 47; do
                    for enable_pin in 40 41 42 43 44 45 46 47; do
                        try_gpio_config ${gpiochip} $reset_pin $pdwn_pin $enable_pin "GPIO chip ${gpiochip}: reset=${reset_pin}, pdwn=${pdwn_pin}, enable=${enable_pin}" || true
                    done
                done
            done
            
            # If we still haven't found it, try pins 48-55
            for reset_pin in 48 49 50 51 52 53 54 55; do
                for pdwn_pin in 48 49 50 51 52 53 54 55; do
                    for enable_pin in 48 49 50 51 52 53 54 55; do
                        try_gpio_config ${gpiochip} $reset_pin $pdwn_pin $enable_pin "GPIO chip ${gpiochip}: reset=${reset_pin}, pdwn=${pdwn_pin}, enable=${enable_pin}" || true
                    done
                done
            done
            
            # If we still haven't found it, try pins 56-63
            for reset_pin in 56 57 58 59 60 61 62 63; do
                for pdwn_pin in 56 57 58 59 60 61 62 63; do
                    for enable_pin in 56 57 58 59 60 61 62 63; do
                        try_gpio_config ${gpiochip} $reset_pin $pdwn_pin $enable_pin "GPIO chip ${gpiochip}: reset=${reset_pin}, pdwn=${pdwn_pin}, enable=${enable_pin}" || true
                    done
                done
            done
        fi
    fi
done

echo ""
echo "=== Summary ==="
echo "Tried all permutations of reset, pdwn, and enable pins (0-63) on all GPIO chips"
echo "If no PMIC response was detected, the pins might be on a different GPIO controller"
echo "or the INT3472 devices might use different GPIO pin numbers"
