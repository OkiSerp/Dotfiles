#!/usr/bin/env sh

while true; do
  keyboard="$(~/.dotfiles/scripts/keyboard)"
  volume="$(~/.dotfiles/scripts/volume --status)"
  battery="$(~/.dotfiles/scripts/battery)"
  brightness="$(~/.dotfiles/scripts/brightness --status)"
  datetime="$(date '+%b %d %a') $(date '+%R')"

  xsetroot -name "$keyboard / $volume / $battery / $brightness / $datetime"

  sleep 750ms
done &

xrandr --size 1920x1080 &

xset r rate 350 55 &

xset s off -dpms &

xinput set-prop "Synaptics TM3336-001" "libinput Tapping Enabled" 1 &

xbanish &

sxhkdrc="$HOME/.config/sxhkd/sxhkdrc"
wallpaper="$HOME/.local/share/dwm/wallpaper"

test -e $keysfile && sxhkd -c $sxhkdrc &

[[ -e $wallpaper ]] && feh --no-fehbg --bg-scale $wallpaper &

sh -c "~/.dotfiles/scripts/keyboard --init" &
