# Asus N551JK

Great from 2016-2020. Broken display anchor at some point.

## ecryptfs recovery

did't want to share credentials with my agent, so i tried proposed commands, without success

    sudo /usr/bin/ecryptfs-mount-private /run/media/jan/ubuntu/home/.ecryptfs/jan/.Private /home/jan/recovered_home/
    sudo ecryptfs-recover-private /run/media/jan/ubuntu/home/.ecryptfs/jan/.Private

    mount: /tmp/ecryptfs.Lbqxz3yH: mount() fehlgeschlagen: Datei oder Verzeichnis nicht gefunden.
       dmesg(1) könnte nach einem fehlgeschlagenen mount-Systemaufruf
       weitere Informationen liefern.
    ERROR: Failed to mount private data at [/tmp/ecryptfs.Lbqxz3yH].

    # the secret ingredient
    sudo ecrypt-manager # do nothing but exit

    # above starts to work

<https://askubuntu.com/questions/613691/cant-access-my-home-directory-encrypted-private-directory-is-not-setup-properl>
