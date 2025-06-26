#!/bin/bash
# Usage: remindme -t "3m 1s" -m "I have to do homework"
# Usage: remindme "message" (sends immediately)

# If no flags provided, treat first argument as immediate message
if [[ $# -eq 1 && $1 != -* ]]; then
    notify-send -a Reminder "$1"
    exit 0
fi

while getopts "t:m:" optname; do
case $optname in
t)
time=${OPTARG}
;;
m)
message=${OPTARG}
;;
esac
done

if [ -z "$time" ]; then
    notify-send -a Reminder "$message"
else
    coproc (sleep $time && notify-send -a Reminder "$message")
fi
