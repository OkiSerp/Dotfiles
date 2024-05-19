#!/usr/bin/env sh

while true; do
  key="$(~/.local/share/dwm/scripts/keyboard)"
  vol="$(~/.local/share/dwm/scripts/volume)"
  brt="$(~/.local/share/dwm/scripts/brightness)"
  bat="$(~/.local/share/dwm/scripts/battery)"
  dat="$(~/.local/share/dwm/scripts/datetime)"

  xsetroot -name "${key} / ${vol} / ${brt} / ${bat} / ${dat}"

  sleep 2s
done &

xrandr --size 1920x1080 &

xset r rate 350 55 &

xset s off -dpms &

xinput set-prop "Synaptics TM3336-001" "libinput Tapping Enabled" 1 &

xbanish &

sxhkdrc="$HOME/.config/sxhkd/sxhkdrc"
wallpaper="$HOME/.local/share/dwm/wallpaper"

test -e "${sxhkdrc}" && sxhkd -c "${sxhkdrc}" &

if [[ -e "${wallpaper}" ]];then
  feh --no-fehbg --bg-scale "${wallpaper}" &
fi

setxkbmap -layout us,ua -option grp:shifts_toggle &
