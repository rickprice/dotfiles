#! /usr/bin/env bash

typeset -i i END START

let NUMBER_TAGS=8

TEMPORARY_DIRECTORY=$(mktemp -d)

echo "Temporary Directory is:" $TEMPORARY_DIRECTORY

FORMAT_STRING="%05d"

let i=0
while ((i<NUMBER_TAGS)); do
    TAG_NUMBER=$(printf $FORMAT_STRING $i)
    cp BagTag.pdf $TEMPORARY_DIRECTORY/BagTag-$TAG_NUMBER.pdf

    let i++
done

pdfjam -o bag_tags.pdf --nup 2x4 --landscape $TEMPORARY_DIRECTORY/*.pdf
