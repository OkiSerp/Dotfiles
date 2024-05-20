#!/usr/bin/env sh

battery() {
  cap="$(cat /sys/class/power_supply/BAT0/capacity)"
  stat="$(cat /sys/class/power_supply/BAT0/status)"

  icon="󰂃"
  [[ "${stat}" == "Charging" ]] && icon="󰂄"

  printf "%s %s%%" "${icon}" "${cap}"
}

brightness() {
  brt="$(calc "100 * $(brightnessctl get) / $(brightnessctl max)")"

  printf "%s %.*f%%" "󱠂" 0 "${brt:2:-1}"
}

datetime() {
  date="$(date '+%b %d %a')"
  time="$(date '+%R')"

  printf "%s %s" "${date}" "${time}"
}

wlan() {
  case "$(cat /sys/class/net/wl*/operstate 2> /dev/null)" in
    up)
      stat="󰤨 Up"
      ;;
    down)
      stat="󰤭 Dn"
      ;;
  esac

  printf "%s" "${stat}"
}

layout() {
  key="$(xset -q | grep "LED" | awk "{ print \$10 }")"

  label="us"
  [[ "${key}" == "00001000" ]] && label="ua"

  printf "%s %s" "󰘲" "${label}"
}

volume() {
  mute="$(pactl get-sink-mute @DEFAULT_SINK@)"
  vol="$(pactl get-sink-volume @DEFAULT_SINK@ \
    | grep "Volume:" | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,')"

  icon="󱄡" && [[ $mute = "Mute: yes" ]] && icon="󰸈"

  printf "%s %s%%" "${icon}" "${vol}"
}

while true; do
  sleep 1s && \
    xsetroot -name "$(layout) / $(volume) / $(brightness) / $(battery) / $(wlan) / $(datetime)"
done
