#! /usr/bin/sh

response=$(timew | awk '$1 == "Current" {print "Running"} $1 == "There" {print "Idle"}')

case $response in
    "Running" ) timew stop :quiet; notify-send "Stopping TimeWarrior tracking." ;;
    "Idle" ) timew continue :quiet; notify-send "Starting TimeWarrior tracking." ;;
esac
