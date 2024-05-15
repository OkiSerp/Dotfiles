#!/usr/bin/env fish

argparse --ignore-unknown \
  "u/up" "d/down" "l/level=" "s/set=" "t/status" -- $argv

not type -q brightnessctl && exit

set -q _flag_set && brightnessctl --quiet set "$_flag_set%" && exit

if set -q _flag_level
  if set -q _flag_up
    brightnessctl --quiet set "+$_flag_level%"
  else if set -q _flag_down
    brightnessctl --quiet set "$_flag_level%-"
  end
  exit
end

function print_status
  set -l current (brightnessctl get) && set -l maximum (brightnessctl max)
  set -l brightness (math "100 * $current / $maximum")
  set -l icon "󰃟"

  printf "%s %s%%" $icon (math "round($brightness)")
end

if set -q _flag_status
  print_status
end
