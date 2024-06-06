#!/bin/sh
#
# Prints out the current quarter, and the week in the quarter
#

#!/bin/bash

set -e -o pipefail

export PIPENV_VENV_IN_PROJECT=1
export PIPENV_PIPFILE=~/.dotfiles/bin/BE_Sprint_VENV/Pipfile
export SPRINTPROGRAM=~/.dotfiles/bin/BE_Sprint_VENV/current_BE_sprint.py
exec pipenv run python $SPRINTPROGRAM
