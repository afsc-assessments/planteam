#!/bin/zsh
set -eu
BUILDER_ROOT="${0:A:h}"
if /usr/bin/python3 "$BUILDER_ROOT/scripts/manage-builder.py" start; then
  /usr/bin/open "http://127.0.0.1:3847/"
else
  echo "The builder could not start. Read the message above; your documents are preserved."
  read -r "?Press Return to close. "
  exit 1
fi
