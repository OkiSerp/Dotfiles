#!/usr/bin/env fish

type -q reflector || exit

sudo reflector -p https -l 5 --sort rate -c UA,PL \
  --verbose --save /etc/pacman.d/mirrorlist
