#!/usr/bin/env sh

while true; do
  vol="VOL $(pactl get-sink-volume @DEFAULT_SINK@ | grep 'Volume:' | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,')%"
  bat="BAT $(cat /sys/class/power_supply/BAT0/capacity)%"
  date="$(date '+%R')"
  sbar="${vol} | ${bat} | ${date}"
  xsetroot -name "${sbar}"
  sleep 1s
done &

xrandr --size 1920x1080 &

xset r rate 350 55 &

xinput set-prop "Synaptics TM3336-001" "libinput Tapping Enabled" 1 &

sxhkd -c ~/.config/sxhkd/sxhkdrc &

wall="${HOME}/.myhome.d/pictures/walls/index"
if [[ -f ${wall} ]]; then
  feh --no-fehbg --bg-scale ${wall} &
fi
