#!/usr/bin/env sh

while true; do
  vol="$(~/.scripts/volumestatus.sh)"
  bat="$(~/.scripts/battery.sh)"
  brt="󰃟 $(( 100 * "$(brightnessctl get)" / "$(brightnessctl max)" ))%"
  sbar="${vol} | ${bat} | ${brt} | 󱑎 $(date '+%R')"
  xsetroot -name "${sbar}"
  sleep 1s
done &

xrandr --size 1920x1080 &

xset r rate 350 55 &

xinput set-prop "Synaptics TM3336-001" "libinput Tapping Enabled" 1 &

xrdb -merge ~/.Xresources

xbanish &

if [[ -f "${HOME}/.config/sxhkd/sxhkdrc" ]]; then
  sxhkd -c "${HOME}/.config/sxhkd/sxhkdrc"  &
fi

wall="${HOME}/.local/share/dwm/wallpaper"
if [[ -f ${wall} ]]; then
  feh --no-fehbg --bg-scale ${wall} &
fi
