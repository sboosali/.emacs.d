#!/bin/bash
set -e
#######################################

NIX_FILE=./default.nix

NIX_RESULT_DIRECTORY=result

#######################################

EMACS_INITIALIZATION_FILE=./init.el

#EMACS_OPTIONS=(-q )

#######################################

nix-build "$NIX_FILE" -o "$NIX_RESULT_DIRECTORY"

#######################################