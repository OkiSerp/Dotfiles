#!/usr/bin/env fish

argparse --ignore-unknown --max-args=1 \
  "t/tor" "p/private" "n/new-window" -- $argv

type -q brave || exit

if set -q _flag_tor
  brave --tor
else if set -q _flag_private
  brave --incognito
else if set -q _flag_new-window
  brave --new-window
else
  brave
end
