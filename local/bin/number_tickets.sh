#! /usr/bin/env bash

typeset -i i

# NUMBER_ARRAY=$(seq 1 64)
NUMBER_ARRAY=(3 4 12 14 16 18)

# NUP='1x4'
NUP='2x5'

FORMAT_STRING="%05d"

TEMPORARY_DIRECTORY=$(mktemp -d)

# echo "Number Array is:" $NUMBER_ARRAY
echo "Temporary Directory is:" $TEMPORARY_DIRECTORY

for i in ${NUMBER_ARRAY[@]}; do
    TICKET_NUMBER=$(printf $FORMAT_STRING $i)
    # echo $TICKET_NUMBER

    sed "s/#TicKet#/$TICKET_NUMBER/g" EditedTicket.svg | inkscape --pipe --export-type=pdf --export-filename=$TEMPORARY_DIRECTORY/Ticket-$TICKET_NUMBER.pdf

done

pdfjam -o EditedTickets.pdf --nup $NUP --landscape $TEMPORARY_DIRECTORY/*.pdf
