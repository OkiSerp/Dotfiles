#!/usr/bin/env sh

sh -c "~/.local/share/dwm/statusbar.sh" &

xrandr --size 1920x1080 &

xset r rate 350 55 &

xset s off -dpms &

xinput set-prop "Synaptics TM3336-001" "libinput Tapping Enabled" 1 &

xbanish &

sxhkdrc="$HOME/.config/sxhkd/sxhkdrc"
wallpaper="$HOME/.local/share/dwm/wall.jpg"

test -e "${sxhkdrc}" && sxhkd -c "${sxhkdrc}" &

if [[ -e "${wallpaper}" ]];then
  feh --no-fehbg --bg-scale "${wallpaper}" &
fi

setxkbmap -layout us,ua -option grp:shifts_toggle &
