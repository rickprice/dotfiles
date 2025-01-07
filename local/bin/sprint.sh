#!/bin/sh
#
# Prints out the current quarter, and the week in the quarter
#

# When this is broken, this seems to fix things:
#
# export SHELL=bash
# bash
# export PIPENV_VENV_IN_PROJECT=1
# export PIPENV_PIPFILE=~/.dotfiles/bin/BE_Sprint_VENV/Pipfile
# export SPRINTPROGRAM=~/.dotfiles/bin/BE_Sprint_VENV/current_BE_sprint.py
# pipenv install

#!/bin/bash

set -e -o pipefail

export PIPENV_VENV_IN_PROJECT=1
export PIPENV_PIPFILE=~/.dotfiles/bin/BE_Sprint_VENV/Pipfile
export SPRINTPROGRAM=~/.dotfiles/bin/BE_Sprint_VENV/current_BE_sprint.py
exec pipenv run python $SPRINTPROGRAM
