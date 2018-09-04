#!/bin/bash
set -e
#################################################

NIX_FILE=./default.nix

NIX_DIRECTORY=result

NIX_EMACS=./"$NIX_DIRECTORY"/bin/emacs 

#################################################

EMACS_DIRECTORY=.emacs-interpreter.d

EMACS_FILE=./init.el

#EMACS_OPTIONS=(-q )
#EMACS_FILE="$EMACS_DIRECTORY"/init.el

#################################################

nix-build "$NIX_FILE" -o "$NIX_DIRECTORY"

#################################################

"$NIX_EMACS"  --no-init-file  --load "$EMACS_FILE"  --chdir="$EMACS_DIRECTORY"  --batch  --eval "$@"

#################################################
# USAGE
#################################################
#
# $ ./eval.sh '(print `((load-file-name . ,(or load-file-name (buffer-file-name))) (default-directory . ,default-directory) (which-emacs . ,which-emacs)))'
#
#   ((load-file-name)
#    (default-directory . ~/.emacs.d/.emacs-interpreter.d/)
#    ...)
#
########################
#
#################################################
# NOTES
#################################################
#
# `--batch`
#
# Run Emacs in batch mode.
#
# Batch mode is used for running programs written in Emacs Lisp from shell scripts, makefiles, and so on. To invoke a Lisp program, use the ‘-batch’ option in conjunction with one or more of ‘-l’, ‘-f’ or ‘--eval’.
#
# In batch mode:
# - Emacs does not display the text being edited, and the standard terminal interrupt characters such as C-z and C-c have their usual effect.
# - Emacs functions that normally print a message in the echo area will print to either the standard output stream (stdout) or the standard error stream (stderr) instead. (To be precise, functions like prin1, princ and print print to stdout, while message and error print to stderr.)
# - Functions that normally read keyboard input from the minibuffer take their input from the terminal's standard input stream (stdin) instead.
#
# See:
#
# - https://www.gnu.org/software/emacs/manual/html_node/emacs/Initial-Options.html
#
#
########################
#
#
#
#################################################