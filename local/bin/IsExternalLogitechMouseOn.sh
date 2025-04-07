#!/usr/bin/env bash

if xinput list --name-only | grep -q -e "Logitech M" ; then
    echo "External Logitech mouse plugged in"
    exit 0
else
    echo "External Logitech mouse not plugged in"
    exit 1
fi
