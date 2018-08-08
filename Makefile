##################################################
run:
	@exec ./run.sh

##################################################
build:
	@exec ./build.sh

##################################################
install:
	@exec ./install.sh #TODO

##################################################
run-quiet:
	@exec ./quiet.sh

##################################################
.PHONY: run build install run-quiet
##################################################