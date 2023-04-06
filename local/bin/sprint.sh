#!/bin/sh
#
# Prints out the current quarter, and the week in the quarter
#

QUARTER=$(date +'%q')
WEEK=$(date +"%W%%%q+1" | bc)
SPRINT=$(date +"(%W%%%q+1)/2+1" | bc)

echo "Q"$QUARTER"-S"$SPRINT
