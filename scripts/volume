#!/usr/bin/env fish

argparse --ignore-unknown \
  "u/up" "d/down" "m/mute" "l/level=" "s/set=" "t/status" -- $argv

not type -q pactl && exit

if set -q _flag_mute
  pactl set-sink-mute @DEFAULT_SINK@ toggle
  exit
end

if set -q _flag_set
  pactl set-sink-volume @DEFAULT_SINK@ "$_flag_set%"
  exit
end

if set -q _flag_level
  if set -q _flag_up
    pactl set-sink-volume @DEFAULT_SINK@ "+$_flag_level%"
  else if set -q _flag_down
    pactl set-sink-volume @DEFAULT_SINK@ "-$_flag_level%"
  end
  exit
end

function print_status
  set -l mute (pactl get-sink-mute @DEFAULT_SINK@)

  set -l volume (pactl get-sink-volume @DEFAULT_SINK@ | \
    grep "Volume:" | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,')

  set -l icon ""
  if [ $mute = "Mute: yes" ]
    set icon "󰖁"
  else if [ $volume = "0" ]
    set icon "󰖁"
  else
    set icon "󰕾"
  end

  printf "%s %s%%" $icon $volume
end

if set -q _flag_status
  print_status
end
