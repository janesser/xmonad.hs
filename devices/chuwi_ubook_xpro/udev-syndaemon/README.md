# udev rule for syndaemon

## Symptom 

syndaemon binds to a device on startup.
on suspend/resume or reconnect device binding breaks.

```
X Error of failed request:  XI_BadDevice (invalid Device parameter)
  Major opcode of failed request:  131 (XInputExtension)
  Minor opcode of failed request:  37 (X_ChangeDeviceProperty)
  Device id in failed request: 0x17
  Serial number of failed request:  31
  Current serial number in output stream:  32
```

## Mitigation

create udev rule that reacts on create/remove events of devices

criteria will be name: **HS-CH12U-PTP-01-04-14 USB KeyBoard Touchpad**

in order to bind to the respective xsession, udev-rule from root context needs to trigger systemd-user

<https://unix.stackexchange.com/questions/550279/udev-rule-to-trigger-systemd-service>

### Helpful Tooling

  sudo udevadm control --log-priority=debug
  journalctl -f -u systemd-udevd
  # output outlines that the udev-worker runs into a **session-c6.scope**

### FailFasts

  su - $USER -w XAUTHORITY,DISPLAY ... # works from root-shell, not from udev-rule
