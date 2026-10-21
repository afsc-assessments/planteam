#!/bin/zsh
set -eu
BUILDER_ROOT="${0:A:h}"
/usr/bin/python3 "$BUILDER_ROOT/scripts/manage-builder.py" stop
