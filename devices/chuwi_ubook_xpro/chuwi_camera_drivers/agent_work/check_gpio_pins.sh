#!/bin/bash
# Check GPIO configuration and try to map INT347A/INT347E pins

echo "=== GPIO Chips ==="
ls /sys/class/gpio/gpiochip* 2>/dev/null

echo ""
echo "=== GPIO chip512 details ==="
cat /sys/class/gpio/gpiochip512/ngpio 2>/dev/null
ls /sys/class/gpio/gpiochip512/gpio* 2>/dev/null | head -20

echo ""
echo "=== Check exported GPIOs ==="
ls /sys/class/gpio/gpio*/direction 2>/dev/null

echo ""
echo "=== Try to export and read INT347A GPIO 3,4,5 (reset, pdwn, enable) ==="
echo "Exporting GPIO pins on gpiochip512..."
for pin in 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151; do
  echo "Exporting gpiochip512 gpio $pin..."
  echo $pin > /sys/class/gpio/export 2>/dev/null
  echo "gpiochip512 gpio $pin" >> /sys/class/gpio/export 2>/dev/null
done

echo ""
echo "=== Reading all exported GPIOs ==="
for gpio in /sys/class/gpio/gpio*/value; do
  pin=$(basename $gpio)
  dir=$(cat $gpio/direction 2>/dev/null)
  val=$(cat $gpio/value 2>/dev/null)
  if [ "$dir" != "in" ] || [ "$val" != "0" ]; then
    echo "gpio $pin: direction=$dir value=$val"
  fi
done

echo ""
echo "=== Try to unexport all ==="
for gpio in /sys/class/gpio/gpio*/; do
  echo "unexporting $(basename $gpio)..."
  echo "unexport" > $gpio 2>/dev/null
done
