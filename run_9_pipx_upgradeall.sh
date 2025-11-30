#!/bin/bash

poetry cache clear PyPI --all -n
poetry cache clear _default_cache --all -n
pipx upgrade-all