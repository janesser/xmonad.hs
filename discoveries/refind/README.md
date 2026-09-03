# rEFInd

https://piotrnowicki.com/posts/2024-06-17/using-refind-instead-of-grub-as-boot-manager/

  sudo apt install refind
  sudo refind-mkdefault
  # eventually need to enter bios for bootorder

Use vmlinuz directly.

## Uninstall grub

  sudo apt remove --purge grub*
