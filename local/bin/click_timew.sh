#! /usr/bin/sh

response=$(timew | awk '$1 == "Current" {print "Running"} $1 == "There" {print "Idle"}')

case $response in
    "Running" ) timew stop :quiet ;;
    "Idle" ) timew continue :quiet ;;
esac
