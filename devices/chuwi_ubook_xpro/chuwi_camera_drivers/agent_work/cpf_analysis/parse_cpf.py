#!/usr/bin/env python3
"""
Parse Intel CPF/AIQB camera calibration files to extract GPIO configurations.
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
        
        # Look for GPIO-related strings
        gpio_strings = []
        for i in range(len(data) - 4):
            if data[i:i+4] == b'gpio' or data[i:i+4] == b'GPIO':
                gpio_strings.append(f"Found 'gpio' at offset {i}")
        
        # Look for reset-related strings
        reset_strings = []
        for i in range(len(data) - 5):
            if data[i:i+5] == b'reset' or data[i:i+5] == b'RESET':
                reset_strings.append(f"Found 'reset' at offset {i}")
        
        # Look for enable-related strings
        enable_strings = []
        for i in range(len(data) - 6):
            if data[i:i+6] == b'enable' or data[i:i+6] == b'ENABLE':
                enable_strings.append(f"Found 'enable' at offset {i}")
        
        if gpio_strings:
            print(f"GPIO strings found: {len(gpio_strings)}")
            for s in gpio_strings[:10]:
                print(f"  {s}")
        
        if reset_strings:
            print(f"Reset strings found: {len(reset_strings)}")
            for s in reset_strings[:10]:
                print(f"  {s}")
        
        if enable_strings:
            print(f"Enable strings found: {len(enable_strings)}")
            for s in enable_strings[:10]:
                print(f"  {s}")
        
        # Look for pin numbers near GPIO strings
        if gpio_strings:
            print("\nSearching for pin numbers near GPIO strings...")
            for offset in [s.split('at offset ')[-1] for s in gpio_strings if 'at offset ' in s]:
                offset = int(offset)
                # Search for numbers near the GPIO string
                for search_offset in range(offset - 100, offset + 100):
                    if 0 <= search_offset < len(data):
                        for j in range(search_offset, min(search_offset + 5, len(data))):
                            if data[j:j+2] == b'\x31' or data[j:j+2] == b'\x32' or data[j:j+2] == b'\x33' or data[j:j+2] == b'\x34' or data[j:j+2] == b'\x35' or data[j:j+2] == b'\x36' or data[j:j+2] == b'\x37' or data[j:j+2] == b'\x38' or data[j:j+2] == b'\x39':
                                print(f"  Found number near GPIO at offset {search_offset}: {data[j:j+2].hex()}")
        
        return True
    except Exception as e:
        print(f"Error parsing {cpf_path}: {e}")
        return False

if __name__ == '__main__':
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: parse_cpf.py <cpf_file>")
        sys.exit(1)
    
    for cpf_file in sys.argv[1:]:
        parse_cpf_file(cpf_file)
    