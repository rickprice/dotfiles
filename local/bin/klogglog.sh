#!/bin/bash

set -e -o pipefail

RAWLOG=$(mktemp)
LOG=$(mktemp)
RAW_INCOMING_S3=$1
COOKED_INCOMING_S3="${RAW_INCOMING_S3%/*}/logs.jsonl"

aws s3 cp $COOKED_INCOMING_S3 $RAWLOG
jq -r .body.msg $RAWLOG > $LOG
klogg $LOG
rm -f $RAWLOG $LOG
