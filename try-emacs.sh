#!/bin/bash
set -e
#######################################

nix-build -o result-emacs "$@"

./result-emacs/bin/emacs -q --load ./init.el

# e.g.
# ./
