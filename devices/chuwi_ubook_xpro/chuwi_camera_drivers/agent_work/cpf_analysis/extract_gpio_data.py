#!/usr/bin/env python3
"""
Extract GPIO-related data from Ghidra project.
"""

import sys
import os

def extract_gpio_data(project_path):
    """Extract GPIO-related data from Ghidra project."""
    try:
        from ghidra.app.services import HeadlessServiceManager
        from ghidra.app.util.headless import HeadlessAnalyzer
        
        # Load the project
        print(f"Loading Ghidra project: {project_path}")
        
        # Use the project directly
        project_dir = project_path
        print(f"Project directory: {project_dir}")
        
        # List contents
        for item in os.listdir(project_dir):
            print(f"  {item}")
        
        # Check idata directory
        idata_dir = os.path.join(project_dir, "idata")
        if os.path.exists(idata_dir):
            print(f"\nIData directory contents:")
            for item in os.listdir(idata_dir):
                print(f"  {item}")
        
        # Check versioned directory
        versioned_dir = os.path.join(project_dir, "versioned")
        if os.path.exists(versioned_dir):
            print(f"\nVersioned directory contents:")
            for item in os.listdir(versioned_dir):
                print(f"  {item}")
        
        return True
    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: extract_gpio_data.py <project_path>")
        sys.exit(1)
    
    extract_gpio_data(sys.argv[1])
