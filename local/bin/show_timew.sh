#! /usr/bin/sh

echo "TW" $(timew | awk '$1 == "Tracking" {print "[ " $2 " ]" } $1 == "Current" {print $2} $1 == "There" {print "Idle"}')
