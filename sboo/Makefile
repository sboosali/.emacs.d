##################################################
# Settings #######################################
##################################################

SHELL=bash

.EXPORT_ALL_VARIABLES:

##################################################
# Variables ######################################
##################################################

EmacsBuild ?=emacs  --directory=./  -batch  --funcall=batch-byte-compile
EmacsEval  ?=emacs  --directory=./  -batch  -Q  --eval

ElispFiles ?=*.el

# ElispFiles ?=*.el
# ElispFiles ?=sboo-conditions.el sboo-definitions.el sboo-utilities.el sboo-commands.el *.el

# ^ TODO requires topological-sort?

##################################################
# Targets ########################################
##################################################

build:
	emacs  --directory=./ --directory=../lisp  -batch  --funcall=batch-byte-compile  $(ElispFiles)

.PHONY: build

#------------------------------------------------#

color:
	emacs  --directory=./  -batch  --funcall=batch-byte-compile  sboo-color.el

.PHONY: color

#------------------------------------------------#

mtg:
	emacs  --directory=./  -batch  --funcall=batch-byte-compile  mtg.el

.PHONY: mtg

#------------------------------------------------#

clean:
	find ./  -type f  -name '*.elc'  -delete
	find ./  -maxdepth 1  -type f  \( -name '*.alist.el' -o -name '*.list.el' \) -delete

.PHONY: clean

##################################################
# Files ##########################################
##################################################

colornames.alist.el: colornames.json

	./print-colornames > $@

#------------------------------------------------#

scryfall-default-cards.alist.el: scryfall-default-cards.json

	./print-scryfall-cards > $@

##################################################
# EOF ############################################
##################################################