#!/usr/bin/env bash

TOUCHPAD_NAME=$(xinput list --name-only | grep Touchpad)
echo "Touchpad is [$TOUCHPAD_NAME]"

if [ -n "$TOUCHPAD_NAME" ]; then
    if IsExternalLogitechMouseOn ; then
        echo "Disable Touchpad"
        xinput disable "$TOUCHPAD_NAME"
        exit 0
    else
        echo "Enable Touchpad"
        xinput enable "$TOUCHPAD_NAME"
        exit 1
    fi
fi

# Uh, not touchpad found
echo "Touchpad not found"
exit 2

