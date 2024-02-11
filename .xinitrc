#!/usr/bin/env bash

while true; do
    xsetroot -name " $(cat /sys/class/power_supply/BAT0/capacity)% | $(date "+%R") "
    sleep 1m
done &

xrandr --size 1920x1080 &

feh --bg-scale ~/Pictures/Walls/wall-06.jpg &

xset r rate 350 55 &

xinput set-prop "Synaptics TM3336-001" "libinput Tapping Enabled" 1 &

exec dwm
