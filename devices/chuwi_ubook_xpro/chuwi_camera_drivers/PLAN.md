# Implementing chuwi camera drivers

## Principles

- work on one device at a time
- maintain a compilable version
- 'git commit' before applying changes

## Goal 1: code work

[] find a **reference implementation**
  - camera sensor
  - v4l2
  - i2c
  - power toggle over PCI
[] copy **reference implementation** to project directory and compile it
[] driven by above Principles adopt details from acpidump

## Goal 2: integration work

Requires to be executed on the right machine, i.e. **peacewagon**

[] try load kernel module
[] verify 'v4l2-ctl --list-devices'
[] use 'cheese -d <paste device name from v4l2-ctl..>'
