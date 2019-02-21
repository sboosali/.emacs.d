##################################################
# Variables ######################################
##################################################

#Emacs?=emacs
Emacs?=./result/bin/emacs

Cask?=cask

EmacsOptions?=--debug-init --no-desktop --maximized --no-splash --name=SBoo

Timestamp ?=$(shell date +%d-%m-%Y+%H:%M)

##################################################
# Default / Miscellaneous ########################
##################################################
default: run

.PHONY: default

##################################################
clean:
	find sboo/ lisp/  -type f  -name '*.elc'  -exec rm -f \{} \+

.PHONY: clean

##################################################
# Install Dependencies ###########################
##################################################
configure:
	@exec nix-build

.PHONY: configure

##################################################
# Build ##########################################
##################################################
build:
	$(Cask) build

.PHONY: build

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

#
