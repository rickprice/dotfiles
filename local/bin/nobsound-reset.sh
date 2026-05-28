#!/bin/bash
# Reset all Kensington Expert Mouse Trackballs (047d:1020)
lsusb -d 8087:1024 | while read -r _ bus _ dev _; do
    bus="${bus%:}"
    dev="${dev%:}"
    echo "$bus/$dev"
    # sudo usbreset "/dev/bus/usb/$bus/$dev"
    usbreset "$bus/$dev"
done
