##################################################

Bin=./scripts

##################################################
default: run

.PHONY: default

##################################################
run: run-via-package-dot-el

.PHONY: run

##################################################
run-via-package-dot-el:
	@exec emacs

.PHONY: run-via-package-dot-el

##################################################
run-via-nixpkgs:
	@exec ${Bin}/run.sh

.PHONY: run-via-nixpkgs 

##################################################
build:
	@exec ${Bin}/build.sh

.PHONY: build 

##################################################
install:
	@exec ${Bin}/install.sh #TODO


.PHONY: install 

##################################################
run-quiet:
	@exec ${Bin}/quiet.sh

.PHONY: run-quiet

##################################################