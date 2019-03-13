##################################################
# Variables ######################################
##################################################

Emacs      ?=emacs
EmacsBuild ?=emacs  -batch  --funcall=batch-byte-compile

#Emacs ?=emacs
#Emacs ?=./result/bin/emacs

Cask?=cask

EmacsOptions ?=--debug-init --no-desktop --maximized --no-splash --name=SBoo

EmacsDirectory ?=$(CURDIR)

Timestamp ?=$(shell date +%d-%m-%Y+%H:%M)

##################################################
# Default / Miscellaneous ########################
##################################################

default: build
.PHONY: default

##################################################
# Build ##########################################
##################################################

build:

	$(EmacsBuild)  $(EmacsDirectory)/sboo/*.el

.PHONY: build

##################################################
# Configure ######################################
##################################################

# Install Dependencies:

configure:
	@exec nix-build

.PHONY: configure

##################################################
# Cask ###########################################
##################################################

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
	~/.nix-profile/bin/emacs --name=SBoo/Nixpkgs/$(Timestamp) --maximized --no-splash &disown

.PHONY: exec

##################################################

clean:
	find $(EmacsDirectory)/sboo $(EmacsDirectory)/lisp  -type f  -name '*.elc'  -delete

.PHONY: clean

##################################################
