#!/usr/bin/env sh

while true; do
  volume="$(~/.dotfiles/scripts/volume --status)"
  battery="$(~/.dotfiles/scripts/battery)"
  brightness="$(fish -c '~/.dotfiles/scripts/brightness --status')"
  datetime="$(date '+%b %d %a') $(date '+%R')"

  xsetroot -name "$volume / $battery / $brightness / $datetime"

  sleep 700ms
done &

xrandr --size 1920x1080 &

xset r rate 350 55 &

xinput set-prop "Synaptics TM3336-001" "libinput Tapping Enabled" 1 &

xbanish &

keysfile="$HOME/.config/sxhkd/sxhkdrc"
[[ -e $keysfile ]] && sxhkd -c $keysfile &

wallpaper="$HOME/.local/share/dwm/wallpaper"
[[ -e $wallpaper ]] && feh --no-fehbg --bg-scale $wallpaper &
