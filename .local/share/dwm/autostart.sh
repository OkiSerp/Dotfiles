#!/usr/bin/env sh

while true; do
  xsetroot -name "$(sh -c "~/.local/share/dwm/statusbar.sh")"
  sleep 1s
done &

xrandr --size 1920x1080 &

xset r rate 350 55 &

xset s off -dpms &

unclutter --timeout 1 --start-hidden --jitter 32 &

xinput set-prop "Synaptics TM3336-001" "libinput Tapping Enabled" 1 &

setxkbmap -layout us,ua -option grp:shifts_toggle &

sxhkdrc="$HOME/.config/sxhkd/sxhkdrc"
wall="$HOME/.local/share/wallpapers/main.jpg"

test -r "${sxhkdrc}" && sxhkd -c "${sxhkdrc}" &

test -e "${wall}" && feh --no-fehbg --bg-scale "${wall}" &
