#!/bin/bash
set -e
#######################################
# e.g.

# $ ./emacs.sh -q --debug-init
# a.k.a.
# $ ./emacs-result/bin/emacs -q --debug-init

########################################
EMACS_OPTIONS="$@"

########################################

EMACS_RESULTS_DIR=result
# EMACS_RESULTS_DIR=emacs-result

nix-build -o "$EMACS_RESULTS_DIR" --show-trace

"./$EMACS_RESULTS_DIR/bin/emacs" ${EMACS_OPTIONS} 2>/dev/null

########################################

# --file file, --find-file file, --visit file
#         The same as specifying file directly as an argument.

# +number 
#         Go  to  the line specified by number (do not insert a space between the
#         "+" sign and the number).  This applies only to the  next  file  specified.

# +line:column
#         Go to the specified line and column.

# `-q`
# (do not load an initialization file)

# --script file
#         Run file as an Emacs Lisp script.

# -f function, --funcall function
#         Execute the lisp function function.

# -l file, --load file
#         Load the lisp code in the file file.

# --eval expr, --execute expr
#         Evaluate the Lisp expression expr.



########################################