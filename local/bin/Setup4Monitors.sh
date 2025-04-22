#!/bin/sh

numlockx on

xrandr --dpi 120
xrandr --output eDP-1 --primary --mode 2560x1600 --pos 0x1080 --rotate normal --output DP-1 --mode 2560x1080 --pos 2560x1080 --rotate normal --output HDMI-1 --off --output DP-2 --mode 1920x1080 --pos 2560x0 --rotate normal --output DP-3 --off --output DP-4 --off --output DP-5 --off --output DVI-I-2-1 --mode 1600x900 --pos 960x180 --rotate normal

~/.config/autorandr/postswitch.d/10_setup_feh
