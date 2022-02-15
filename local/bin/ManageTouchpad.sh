#!/usr/bin/env bash

TOUCHPAD_NAME=$(xinput list --name-only | grep Touchpad)
echo "Touchpad is [$TOUCHPAD_NAME]"

if [ -n "$TOUCHPAD_NAME" ]; then
    if [[ "$1" == "off" ]] ; then
        notify-send "Disable Touchpad"
        xinput disable "$TOUCHPAD_NAME"
    else
        notify-send "Enable Touchpad"
        xinput enable "$TOUCHPAD_NAME"
    fi
    exit 0
fi

echo "Touchpad not found"
exit 2

