#!/usr/bin/env python3
"""
Advanced CPF/AIQB parser to extract GPIO configurations.
Uses Intel IPU6 camera HAL format knowledge.
"""

import struct
import os

def parse_cpf_file(cpf_path):
    """Parse a CPF file and extract GPIO configurations."""
    try:
        with open(cpf_path, 'rb') as f:
            data = f.read()
        
        print(f"=== Parsing {cpf_path} ===")
        print(f"File size: {len(data)} bytes")
        
        # Look for known CPF format headers
        # AIQB files start with "AIQB"
        if data[:4] == b'AIQB':
            print("File format: AIQB (Intel AIQB calibration)")
        
        # Look for GPIO-related strings with different patterns
        gpio_strings = []
        for i in range(len(data) - 4):
            if data[i:i+4] == b'gpio' or data[i:i+4] == b'GPIO':
                # Find surrounding context
                start = max(0, i - 20)
                end = min(len(data), i + 20)
                context = data[start:end].decode('ascii', errors='ignore')
                gpio_strings.append(f"Offset {i}: ...{context}...")
        
        # Look for pin-related strings
        pin_strings = []
        for i in range(len(data) - 4):
            if b'pin' in data[i:i+4] or b'PIN' in data[i:i+4]:
                start = max(0, i - 20)
                end = min(len(data), i + 20)
                context = data[start:end].decode('ascii', errors='ignore')
                pin_strings.append(f"Offset {i}: ...{context}...")
        
        # Look for powerdown/reset/enable strings
        power_strings = []
        for keyword in [b'powerdown', b'POWERDOWN', b'reset', b'RESET', b'enable', b'ENABLE', b'pdwn', b'PDWN']:
            for i in range(len(data) - len(keyword)):
                if data[i:i+len(keyword)] == keyword:
                    start = max(0, i - 20)
                    end = min(len(data), i + 20)
                    context = data[start:end].decode('ascii', errors='ignore')
                    power_strings.append(f"Offset {i} ({keyword.decode()})...{context}...")
        
        if gpio_strings:
            print(f"\nGPIO strings found: {len(gpio_strings)}")
            for s in gpio_strings[:20]:
                print(f"  {s}")
        
        if pin_strings:
            print(f"\nPin strings found: {len(pin_strings)}")
            for s in pin_strings[:20]:
                print(f"  {s}")
        
        if power_strings:
            print(f"\nPower/Reset/Enable strings found: {len(power_strings)}")
            for s in power_strings[:20]:
                print(f"  {s}")
        
        # Look for GPIO configuration tables
        # Common patterns in AIQB files
        print("\nSearching for GPIO configuration patterns...")
        
        # Search for GPIO controller references
        gpio_ctrl_patterns = [b'GPIO', b'gpio', b'Gpio', b'GPIO_CTRL']
        for pattern in gpio_ctrl_patterns:
            for i in range(len(data) - len(pattern)):
                if data[i:i+len(pattern)] == pattern:
                    # Search for nearby numbers
                    for j in range(max(0, i-50), min(len(data), i+50)):
                        for k in range(j, min(j+5, len(data))):
                            if data[k] in [0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39]:  # 0-9
                                print(f"  GPIO pattern '{pattern.decode()}' at offset {i}, nearby number at offset {k}: {data[k]:02x}")
        
        # Look for device tree compatible strings
        dt_patterns = [b'ovti,ov5648', b'ovti,ov5648', b'ov5648', b'ovti']
        for pattern in dt_patterns:
            for i in range(len(data) - len(pattern)):
                if data[i:i+len(pattern)] == pattern:
                    start = max(0, i - 50)
                    end = min(len(data), i + 50)
                    context = data[start:end].decode('ascii', errors='ignore')
                    print(f"  Device tree pattern '{pattern.decode()}' at offset {i}: ...{context}...")
        
        return True
    except Exception as e:
        print(f"Error parsing {cpf_path}: {e}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == '__main__':
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: parse_cpf.py <cpf_file>")
        sys.exit(1)
    
    for cpf_file in sys.argv[1:]:
        parse_cpf_file(cpf_file)
    