#!/bin/bash

set -e -o pipefail

RAWLOG=$(mktemp)
LOG=$(mktemp)

aws s3 cp $1 $RAWLOG
jq -r .body.msg $RAWLOG > $LOG
nvim -c 'set filetype=log' $LOG
rm -f $RAWLOG $LOG
