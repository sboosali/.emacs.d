#!/bin/bash
set -e
#######################################

RESULT_DIRECTORY=result-emacs-featured

nix-build -o "$RESULT_DIRECTORY" "$@"

./"$RESULT_DIRECTORY"/bin/emacs -q --load ./nix/featured/init.el

# nix/featured/run.sh

# e.g.
# ./
