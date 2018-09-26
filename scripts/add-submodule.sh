#!/bin/bash
set -e

#################################################
# Utilities #####################################
#################################################

BOLD="$(tput bold)"
UNDERLINE="$(tput smul)"    # {S}et {U}nder{L}ine
STANDOUT="$(tput smso)"     # {S}et {S}tand{O}ut

#################################################

BLACK="$(tput setaf 0)"

RED="$(tput setaf 1)"
GREEN="$(tput setaf 2)"
YELLOW="$(tput setaf 3)"
BLUE="$(tput setaf 4)"

#################################################

RESET="$(tput sgr0)"

RESET_COLOR="$BLACK"

RESET_UNDERLINE="$(tput rmul)" # {R}eset {U}nder{L}ine
RESET_STANDOUT="$(tput rmso)"  # {R}eset {S}tand{O}ut

#################################################

# BOLD="\e[1m"
# UNDERLINE="\e[4m"
# RED='\e[31m'
# GREEN="\e[32m"
# YELLOW="\e[33m"
# BLUE="\e[34m"

#################################################
# Messages ######################################
#################################################

URL="${GREEN}URL${BLACK}"
NAME="${GREEN}NAME${BLACK}"

ERROR="${RED}${BOLD}ERROR${RESET}"

QL="« ${UNDERLINE}"       # left-quote (i.e. open-quote)
QR="${RESET_UNDERLINE} »" # right-quote (i.e. close-quote)

#################################################

Message="${ERROR}... Usage is ${QL}$0 ${URL} [${NAME}]${QR}, which runs ${QL}git submodule add --name ${NAME} ${URL} \"submodule/${NAME}\"${QR}"

#################################################
# Arguments #####################################
#################################################

SubmoduleUrl=${1:?"$Message"}

SubmoduleName=${2:-$(basename "${SubmoduleUrl}")}

#################################################
# Commands ######################################
#################################################

SubmodulePath="submodules/${SubmoduleName}"

#################################################

echo
echo '#################################################'
echo

echo "$URL  = ${SubmoduleUrl}" 
echo "$NAME = ${SubmoduleName}"

echo
echo '#################################################'
echo

(set -x; git submodule add --name "${SubmoduleName}" "${SubmoduleUrl}" "${SubmodulePath}")

echo
echo '#################################################'
echo

(set -x; cat .gitmodules)

echo '#################################################'
echo

#################################################
# Notes #########################################
#################################################

#################################################
# Color
#
# for colored messages that are less-illegible, use Bash's (1) command substitution and (2) string interpolation. e.g.:
# #
# ```bash
# GREEN="\[$(tput setaf 2)\]"
# RESET="\[$(tput sgr0)\]"
# export PS1="${GREEN}my prompt${RESET}> "
# ```
# #
# NOTE: Wrapping the `tput` output in « \[ » and « \] » helps Bash ignore non-printable characters, which lets Basb correctly calculate the size of the prompt.

#################################################
# Command Echoing
#
# `set -x` enables command `echo`ing
#

#################################################
# `git` syntax:
# 
#     git submodule add --name <name> "https://github.com/<user>/<repo>" <subdir>
# 
# e.g.
#
# ~/.emacs.d$ git submodule add --name use-package https://github.com/jwiegley/use-package submodules/use-package
# Cloning into '/home/sboo/.emacs.d/submodules/use-package'...
#
# ~/.emacs.d$ cat .gitmodules 
# [submodule "use-package"]
#         path = submodules/use-package
#         url = https://github.com/jwiegley/use-package

#################################################
# 


#################################################