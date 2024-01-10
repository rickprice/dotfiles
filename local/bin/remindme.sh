#!/bin/bash
# Usage: remindme -t "3m 1s" -m "I have to do homework"
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
coproc (sleep $time && notify-send -a Reminder "$message")
