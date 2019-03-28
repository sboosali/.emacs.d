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

init:
	emacs  --directory=./  -batch  --funcall=batch-byte-compile  sboo-init.el

.PHONY: init

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

#------------------------------------------------#

clean-elisp:

	find ./  -type f  -name '*.elc'  -delete

.PHONY: clean-elisp

##################################################
# Files ##########################################
##################################################
#------------------------------------------------#

mtg-cards.el: scryfall-default-cards.json

	./parse-mtg-cards  >  $@

#------------------------------------------------#

mtg-card-names.list.el: mtg-card-names.txt

	./print-mtg-card-names  >  $@

#------------------------------------------------#

mtg-card-names.txt: scryfall-default-cards.json

	cat $^  |  jq ".[].name"  | sort | uniq  >  $@

#------------------------------------------------#

scryfall-default-cards.alist.el: scryfall-default-cards.json

	./print-scryfall-cards > $@

#------------------------------------------------#

colornames.alist.el: colornames.json

	./print-colornames > $@

#------------------------------------------------#

scryfall-default-cards.json:

	wget "https://archive.scryfall.com/json/scryfall-default-cards.json"

#------------------------------------------------#

colornames.json:

	wget "https://unpkg.com/color-name-list@3.64.0/dist/colornames.json"

#------------------------------------------------#
##################################################
# EOF ############################################
##################################################