##################################################
# Settings #######################################
##################################################

SHELL=bash

.EXPORT_ALL_VARIABLES:

##################################################
# Variables ######################################
##################################################

EmacsDirectory ?=$(CURDIR)

# ^ e.g. « ~/emacs.d/ ».

#------------------------------------------------#

Timestamp:=$(shell date +%d-%m-%Y+%H:%M)

#------------------------------------------------#

Emacs      ?=emacs
EmacsBuild ?=emacs  -batch  --funcall=batch-byte-compile
EmacsStart ?=emacs "--name=Emacs - SBoo/$(Timestamp)" --maximized --no-splash --no-desktop

#Emacs ?=emacs
#Emacs ?=./result/bin/emacs


#------------------------------------------------#

EmacsOptions ?= "--name=SBoo/$(Timestamp)" --maximized --no-splash --no-desktop

# ^ « emacs » (case-insensitive) must be in « --name »,
#   for « wmctrl -a emacs » to work.

#------------------------------------------------#

Cask?=cask

##################################################
# Targets ########################################
##################################################

#------------------------------------------------#
# Emacs -----------------------------------------#
#------------------------------------------------#

build:

	$(EmacsBuild)  $(EmacsDirectory)/sboo/*.el

.PHONY: build

#------------------------------------------------#

rebuild: clean

	$(EmacsBuild)  $(EmacsDirectory)/sboo/*.el

.PHONY: rebuild

#================================================#

start: build

	exec $(EmacsStart) &disown

.PHONY: start

#------------------------------------------------#

emacs-run:

	exec $(EmacsStart) &disown

.PHONY: emacs-run

#------------------------------------------------#

emacs-try:

	SBOO_EMACS_DESKTOP_RESTORE_EAGER=10 SBOO_EMACS_DESKTOP_WRITE=0 $(Emacs) $(EmacsOptions) --debug-init

.PHONY: emacs-try

#------------------------------------------------#

emacs-terminal:

	SBOO_EMACS_DESKTOP_RESTORE_EAGER=10 SBOO_EMACS_DESKTOP_WRITE=0 $(Emacs) $(EmacsOptions) --no-window-system --debug-init

.PHONY: emacs-terminal

#------------------------------------------------#

emacs-debug:

	SBOO_EMACS_DESKTOP_ENABLE=0 $(Emacs) $(EmacsOptions) --debug-init "--find-file=$(EmacsDirectory)/sboo/sboo-init.el"

.PHONY: emacs-debug

#================================================#

emacs-build: build
.PHONY: emacs-build

#------------------------------------------------#

# emacs-test: build

# 	SBOO_EMACS_DESKTOP_RESTORE_EAGER=10 $(Emacs) $(EmacsOptions) --debug-init

# .PHONY: emacs-test

#------------------------------------------------#
# Nix -------------------------------------------#
#------------------------------------------------#

configure:

	@exec nix-build

.PHONY: configure

#------------------------------------------------#
# Cask ------------------------------------------#
#------------------------------------------------#

cask-build:
	$(Cask) build

.PHONY: cask-build

##################################################
# Emacs: Run / Test. #############################
##################################################

run: configure
	@exec $(Emacs) $(EmacsOptions)

.PHONY: run

##################################################

test: configure
	@exec $(Emacs) $(EmacsOptions)

.PHONY: test

##################################################

exec:
	~/.nix-profile/bin/emacs --name=Emacs/SBoo/Nixpkgs/$(Timestamp) --maximized --no-splash &disown

.PHONY: exec

##################################################

clean:
	find $(EmacsDirectory)/sboo $(EmacsDirectory)/lisp  -type f  -name '*.elc'  -delete

.PHONY: clean

##################################################