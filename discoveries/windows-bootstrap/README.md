# Windows bootstrap (HP Z6 G4)

Windows-specific setup for the HP Z6 G4. Moved here from
`devices/hp_z6_g4/system.md`.

## bios reset procedure

PWSD jumper
turn on
reboot

in admin-powershell

    BiosConfigUtil64 /get
    # "Admin Password Set" will be shown with "No"

## scoop installation

Scoop is the package manager used on this machine. Installed in a PowerShell
session (run as the normal user):

    # Add the buckets
    scoop bucket add main
    scoop bucket add java
    scoop bucket add extras
    scoop bucket add common

All Scoop apps live under `~/scoop/apps` and `~/scoop/shims`, so they are
available to every user without admin rights. Update everything with
`scoop update *` and list installed apps with `scoop list`.

### installed packages

Current Scoop install (run `scoop list` to refresh):

| App | Version | Source | Purpose |
|-----|---------|--------|---------|
| git | 2.55.0.5 | main | version control |
| 7zip | 26.03 | main | archiver |
| python | 3.14.7 | main | Python runtime + pip |
| uv | 0.12.12 | main | fast Python package/env manager |
| nodejs-lts | 24.21.0 | main | JavaScript runtime |
| bun | 1.4.2 | main | fast JS/TS runtime |
| helix | 25.07.1 | main | modal text editor |
| ghidra | 12.1.3 | extras | reverse engineering |
| microsoft-lts-jdk | 25.0.4.1 | java | JDK (required to run Ghidra) |
| vcredist2022 | 14.51.36247.0 | extras | VC++ shared runtime DLLs |
| dark | 3.14.1 | main | Visual Studio "Dark" theme installer |
| scoop-search | 2.1.0 | main | fuzzy `scoop search` |
| unigetui | 2026.2.7 | extras | game installer |

MVP set (reproduce the box's core job — AI + dev + RE — on a fresh install):

    scoop bucket add main java extras
    scoop install git 7zip python uv nodejs-lts helix ghidra vcredist2022

The JDK and vcredist are real dependencies of Ghidra, so they belong in the
MVP. `dark`, `scoop-search`, `bun`, and `unigetui` are optional extras; drop
them from a bare reinstall. `unigetui` (game installer) is non-essential.

Full set — MVP plus every current extra:

    scoop bucket add main java extras common
    scoop install \
        git \
        7zip \
        python \
        uv \
        nodejs-lts \
        bun \
        helix \
        ghidra \
        microsoft-lts-jdk \
        vcredist2022 \
        dark \
        scoop-search \
        unigetui

## working with git

Git on this machine is provided by Git for Windows.

The `error: invalid path` checkout failure on Windows (issue
[#2803](https://github.com/git-for-windows/git/issues/2803)) happens when a
path contains characters Git refuses to store on the NTFS filesystem. Fix it
locally in this repo:

    git config core.protectNTFS false

Set it globally instead (all repos) with:

    git config --global core.protectNTFS false

### sparse checkout

Sparse checkout is also used here to work around the `error: invalid path`
checkout failure (see [#2803](https://github.com/git-for-windows/git/issues/2803))
by keeping only the needed subtrees checked out.

Use sparse checkout to pull only part of a large repo (e.g. a single
subdirectory) into the working tree. Git for Windows (>= 2.25) ships the
built-in `git sparse-checkout` commands:

    # enable the feature
    git sparse-checkout enable

    # define what to include (relative paths, supports wildcards)
    git sparse-checkout set devices/hp_z6_g4/

    # add or remove paths (this also re-syncs the working tree)
    git sparse-checkout set devices/ docs/

    # list the current patterns
    git sparse-checkout list

To pull the entire repo again, disable it:

    git sparse-checkout disable
