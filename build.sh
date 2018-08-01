#!/bin/bash
set -e
#################################################

NIX_FILE=./default.nix

NIX_DIRECTORY=result

NIX_EMACS=./"$NIX_DIRECTORY"/bin/emacs 

#################################################

EMACS_DIRECTORY=.emacs-interpreter.d

EMACS_FILE=./init.el

#################################################

if   [ "$#" -gt 0 ]
     # ^ whether any command-line-arguments were passed.
then
    ARGUMENTS="$@"
else
    ARGUMENTS="(print (require 'haskell-mode))"
    # ^ check that a package from `packages.nix` is present,
    # i.e. that `nix-shell` correctly provisioned it.
fi
    ###OLD ARGUMENTS='(print `((haskell-mode . (require "'"haskell-mode t nil))))'

#################################################

nix-build "$NIX_FILE" -o "$NIX_DIRECTORY"

#################################################

"$NIX_EMACS" --no-init-file --batch --eval "$ARGUMENTS"

#################################################
# USAGE #########################################
#################################################
#
# Example output (Success):
#
# $ make build
#     
#     /nix/store/2lgly7jm79y5k7ndmsz2gs738b7mmfhc-emacs-with-packages-26.1
#     Loading /nix/store/q7541ph9clzkvrkjxn781ig4c5jpsck8-emacs-26.1/share/emacs/site-lisp/site-start.el (source)...
#     
#     haskell-mode
#
# Example output (Failure):
#
# $ make build
#     
#     /nix/store/2lgly7jm79y5k7ndmsz2gs738b7mmfhc-emacs-with-packages-26.1
#     Loading /nix/store/q7541ph9clzkvrkjxn781ig4c5jpsck8-emacs-26.1/share/emacs/site-lisp/site-start.el (source)...
#     
#     Cannot open load file: No such file or directory, haskell-mode
#     
#     Makefile:7: recipe for target 'build' failed
#     make: *** [build] Error 255
#
#
#
#
#################################################
# NOTES #########################################
#################################################
#
#
#
#
#
#
#
#
#
#
#
#
# See:
#
# - 
#
#
#################################################