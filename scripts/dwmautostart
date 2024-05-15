#!/usr/bin/env fish

while true
  set -l volume (~/.dotfiles/scripts/volume --status)
  set -l battery (~/.dotfiles/scripts/battery)
  set -l brightness (~/.dotfiles/scripts/brightness --status)
  set -l datetime " $(date '+%b %d %a') /  $(date '+%R')"

  xsetroot -name "$volume / $battery / $brightness / $datetime"

  sleep 700ms
end &

xrandr --size 1920x1080 &

xset r rate 350 55 &

xinput set-prop "Synaptics TM3336-001" "libinput Tapping Enabled" 1 &

xbanish &

set -l keysfile $HOME/.config/sxhkd/sxhkdrc
test -e $keysfile && sxhkd -c $keysfile &

set -l wallpaper $HOME/.local/share/dwm/wallpaper
test -e $wallpaper && feh --no-fehbg --bg-scale $wallpaper &
