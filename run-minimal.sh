#!/bin/bash
set -e
#######################################

NIX_FILE=./profiles/minimal/nix/default.nix

INITIALIZATION_FILE=./profiles/minimal/emacs/init.el

#=./profiles/minimal/emacs/elisp/

#EMACS_OPTIONS=(-q )

RESULT_DIRECTORY=result-minimal

#######################################

nix-build "$NIX_FILE" -o "$RESULT_DIRECTORY"

./"$RESULT_DIRECTORY"/bin/emacs -q --load "$INITIALIZATION_FILE" "$@"

#######################################
