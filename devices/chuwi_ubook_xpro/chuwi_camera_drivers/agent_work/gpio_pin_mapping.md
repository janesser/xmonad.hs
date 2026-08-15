# TPS68470 GPIO Pin Mapping - Chuwi UBook XPro

## Extracted from SkcController.sys Binary Analysis

### General GPIO Pins (gpio.0 - gpio.6)

| Pin Number | Function | Binary Function Name | String Address |
|------------|----------|---------------------|----------------|
| 0 | Reset | tps68470::TPS68470::ResetControlLogic | 140018740 |
| 1 | Enable | tps68470::Tps68470Clock::IsEnable | 140018980 |
| 2 | Strobe | tps68470::Tps68470Flash::FlashWithStrobeInitialize | 140018b60 |
| 3 | Torch | discrete::DiscreteControl::TorchOn | 140017ed0 |
| 4 | Flash | tps68470::Tps68470Flash::FlashPowerOn | 140018be0 |
| 5 | LedRear | tps68470::SSTps68470::ExcPrivacyLEDState | 1400182c0 |
| 6 | LedFront | - | - |

### Sensor-specific GPIO Pins

| Pin Number | Function | Binary Function Name | String Address |
|------------|----------|---------------------|----------------|
| 7 | PrivateLED | up6641::uP6641::ExcPrivacyLEDState | 140019690 |
| 8 | Power0 | - | 14001b214 |
| 9 | Power1 | - | 14001b21c |
| 10 | Standby | - | 14001b228 |
| 11 | WriteProtect | - | 14001b230 |
| 12 | PowerEn | - | 14001b240 |
| 13 | Mclk | tps68470::SSCrdG2TiSensor::MclkOutput | 1400183b0 |

### UP6641 GPIO Pins (Camera 2 - OV5648)

| Pin Number | Function | Binary Function Name | String Address |
|------------|----------|---------------------|----------------|
| 0 | Reset | up6641::uP6641::ResetControlLogic | 140019440 |
| 1 | Enable | up6641::uP6641Clock::IsEnable | 1400196e0 |
| 2 | Strobe | up6641::uP6641Flash::FlashWithStrobeInitialize | 140019860 |
| 3 | Torch | up6641::uP6641::TorchOn | 140019570 |
| 4 | Flash | up6641::uP6641Flash::FlashPowerOn | 1400198d0 |
| 5 | LedRear | - | - |
| 6 | LedFront | - | - |

## GPIO Configuration Functions

### CradPoint Gpio Operations (tps68470)
- **CrdGTiGpio::GpioOper** (140018d30) - Camera 1 GPIO operations
- **CrdG2TiGpio::GpioOper** (140018d50) - Camera 2 GPIO operations
- **CrdG2TiQuantaGpio::GpioOper** (140018d70) - Quanta sensor GPIO operations

### SetGpio Functions (tps68470)
- **SSCrdG2TiSensor::SetGpio** (1400183e0) - Sensor GPIO configuration
- **CrdGTiSensor::SetGpio** (140018f00) - Sensor GPIO configuration
- **CrdG2TiSensor::SetGpio** (140019010) - Sensor GPIO configuration

### Sensor Power Functions
- **SSCrdG2TiSensor::SensorPowerOn** (140018350) - Sensor power on
- **SSCrdG2TiSensor::SensorPowerOff** (140018380) - Sensor power off
- **CrdGTiSensor::SensorPowerOn** (140018e70) - Sensor power on
- **CrdGTiSensor::SensorPowerOff** (140018ea0) - Sensor power off
- **CrdG2TiSensor::SensorPowerOn** (140018f80) - Sensor power on
- **CrdG2TiSensor::SensorPowerOff** (140018fb0) - Sensor power off

## Usage Notes

1. **GPIO Direction**: Pins 0-6 are output (GPIOF_OUT_INIT_LOW), pins 7-13 are input (GPIOF_IN)
2. **Reset Sequence**: Assert reset (0) for 10ms, then deassert (1) and wait 100ms
3. **Power Sequence**: Enable power (1), wait 100ms for stabilization
4. **Flash/Torch**: Both Flash and Torch pins must be asserted together
5. **MCLK**: Clock output is configured separately from GPIO pins

## Reference

- TPS68470 Data Sheet: https://www.ti.com/product/TPS68470
- Kernel Driver: drivers/gpio/gpio-tps68470.c
- Binary: SkcController.sys (Chuwi UBook XPro camera driver)
