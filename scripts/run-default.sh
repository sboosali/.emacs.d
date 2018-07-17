#!/bin/bash
set -e
#######################################

NIX_FILE=./default.nix

INITIALIZATION_FILE=./init.el

#EMACS_OPTIONS=(-q )

RESULT_DIRECTORY=result-default

#######################################

nix-build "$NIX_FILE" -o "$RESULT_DIRECTORY" "$@"

./"$RESULT_DIRECTORY"/bin/emacs -q --load "$INITIALIZATION_FILE"

#######################################

