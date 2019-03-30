##################################################
# Variables ######################################
##################################################

Emacs      ?=emacs
EmacsBuild ?=emacs  -batch  --funcall=batch-byte-compile

#Emacs ?=emacs
#Emacs ?=./result/bin/emacs

Cask?=cask

EmacsOptions ?=--debug-init --no-desktop --maximized --no-splash --name=Emacs/SBoo

EmacsDirectory ?=$(CURDIR)

Timestamp ?=$(shell date +%d-%m-%Y+%H:%M)

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

	exec $(Emacs) $(EmacsOptions) &disown

.PHONY: start

#------------------------------------------------#

emacs-run:

	exec $(Emacs) $(EmacsOptions) &disown

.PHONY: emacs-run

#------------------------------------------------#

emacs-try:

	SBOO_EMACS_DESKTOP_RESTORE_EAGER=10 $(Emacs) $(EmacsOptions) --debug-init

.PHONY: emacs-try

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
