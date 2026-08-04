# systemd user unit + udev rule for syndaemon

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

create systemd user service and udev rule that reacts on create/remove events of devices

criteria will be name: **HS-CH12U-PTP-01-04-14 USB KeyBoard Touchpad**

in order to bind to the respective xsession, udev-rule from root context needs to trigger systemd-user

<https://unix.stackexchange.com/questions/550279/udev-rule-to-trigger-systemd-service>

### Helpful Tooling

  sudo udevadm control --log-priority=debug
  journalctl -f -u systemd-udevd
  # output outlines that the udev-worker runs into a **session-c6.scope**

### FailFasts

  su - $USER -w XAUTHORITY,DISPLAY ... # works from root-shell, not from udev-rule

## Files

| File | Purpose |
|---|---|
| `99-syndaemon-restart.rules` | udev rule: sets `ENV{SYSTEMD_USER_WANTS}` on touchpad add/remove |
| `restart-syndaemon` | script: kill + restart syndaemon as user (runs via systemd) |
| `syndaemon-restart.service` | systemd user unit: runs the script as the user |

## Architecture

1. **udev rule** detects touchpad add/remove and sets `ENV{SYSTEMD_USER_WANTS}`
2. **systemd** reads the environment variable and starts `syndaemon-restart.service` as the user
3. **Script** kills existing syndaemon and restarts it
4. **xmonadrc.sh** starts `sysdaemon-restart.service` on user's xsession start

## Usage

### Manual (user session)

```bash
systemctl --user restart syndaemon-restart
```

### From udev rule (root context)

The udev rule sets `ENV{SYSTEMD_USER_WANTS}="syndaemon-restart.service"`, which triggers systemd to start the service automatically.
