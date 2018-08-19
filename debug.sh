#!/bin/bash
set -e
##############################################################################

NIX_FILE=./default.nix

NIX_DIRECTORY=result

#######################################

EMACS_INITIALIZATION_FILE=./init.el

EMACS_EXECUTABLE=./"$NIX_DIRECTORY"/bin/emacs

#EMACS_OPTIONS=(-q )

##############################################################################

nix-build "$NIX_FILE" -o "$NIX_DIRECTORY"

#######################################

"${EMACS_EXECUTABLE}" -q --no-desktop --load "$EMACS_INITIALIZATION_FILE" --debug-init "$@"

##############################################################################
