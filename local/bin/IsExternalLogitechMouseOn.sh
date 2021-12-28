#!/usr/bin/env bash

if solaar show | grep -q -e "Supports.*HID.*features:" ; then
    echo "External Logitech mouse on"
    exit 0
else
    echo "External Logitech mouse off"
    exit 1
fi
