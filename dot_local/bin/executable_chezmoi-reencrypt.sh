#!/bin/bash

chezmoi init # eventually update recipients
chezmoi add --encrypt $(chezmoi managed --include encrypted --path-style absolute)
