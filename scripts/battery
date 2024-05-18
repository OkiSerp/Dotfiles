#!/usr/bin/env fish

set -l capacity (cat /sys/class/power_supply/BAT0/capacity)
set -l battery_status (cat /sys/class/power_supply/BAT0/status)

if [ $battery_status = "Charging" ]
  set -g icons "󰢜" "󰂆" "󰂇" "󰂈" "󰢝" "󰂉" "󰢞" "󰂊" "󰂋" "󰂅"
else
  set -g icons "󰁺" "󰁻" "󰁼" "󰁽" "󰁾" "󰁿" "󰂀" "󰂁" "󰂂" "󰁹"
end

[ $capacity -ge 99 ] && set -l icon $icons[-1]
[ $capacity -lt 99 ] && [ $capacity -ge 90 ] && set -l icon $icons[-2]
[ $capacity -lt 90 ] && [ $capacity -ge 80 ] && set -l icon $icons[-3]
[ $capacity -lt 80 ] && [ $capacity -ge 70 ] && set -l icon $icons[-4]
[ $capacity -lt 70 ] && [ $capacity -ge 60 ] && set -l icon $icons[-5]
[ $capacity -lt 60 ] && [ $capacity -ge 50 ] && set -l icon $icons[-6]
[ $capacity -lt 50 ] && [ $capacity -ge 40 ] && set -l icon $icons[-7]
[ $capacity -lt 40 ] && [ $capacity -ge 30 ] && set -l icon $icons[-8]
[ $capacity -lt 30 ] && [ $capacity -ge 20 ] && set -l icon $icons[-9]
[ $capacity -lt 20 ] && set -l icon $icons[-10]

printf "%s %s%%" $icon $capacity
