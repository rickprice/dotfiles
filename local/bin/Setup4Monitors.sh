#!/bin/sh

xrandr --output eDP-1 --primary --mode 2560x1600 --pos 0x900 --rotate normal

xrandr --output DP-1 --mode 2560x1080 --pos 2560x1080 --rotate normal 

xrandr --output DP-2 --mode 1920x1080 --pos 2560x0 --rotate normal 

xrandr --output DVI-I-2-1 --mode 1600x900 --pos 960x0 --rotate normal
