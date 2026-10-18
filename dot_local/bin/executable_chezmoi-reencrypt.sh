#!/bin/bash

chezmoi init # eventually update recipients

# FIXME this encrypt "LastNotified" of evolution sources
chezmoi add --encrypt $(chezmoi managed --include encrypted --path-style absolute)
