#! /usr/bin/env bash

typeset -i i END START

let START=7
let END=50

FORMAT_STRING="%05d"

TEMPORARY_DIRECTORY=$(mktemp -d)

echo "Temporary Directory is:" $TEMPORARY_DIRECTORY

let i=$START
while ((i<=END)); do
    TICKET_NUMBER=$(printf $FORMAT_STRING $i)
    # echo $TICKET_NUMBER

    sed "s/#TicKet#/$TICKET_NUMBER/" EditedTicket.svg | inkscape --pipe --export-type=pdf --export-filename=$TEMPORARY_DIRECTORY/Ticket-$TICKET_NUMBER.pdf

    let i++
done

pdfjam -o EditedTickets.pdf --nup 1x4 --landscape $TEMPORARY_DIRECTORY/*.pdf
