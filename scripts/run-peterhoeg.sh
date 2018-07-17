#!/bin/bash
set -e
#######################################

NIX_FILE=./peterhoeg/emacs/default.nix

INITIALIZATION_FILE=./peterhoeg/elisp/init.el

RESULT_DIRECTORY=result-peterhoeg

#######################################

nix-build "$NIX_FILE" -o "$RESULT_DIRECTORY"

./"$RESULT_DIRECTORY"/bin/emacs -q --load "$INITIALIZATION_FILE" "$@"

#######################################