#!/usr/bin/env sh

basedir="/sys/class/backlight/intel_backlight/"

old_brightness="$(cat ${basedir}'brightness')"

max_brightness="$(cat ${basedir}'max_brightness')"

old_brightness_p="$(( 100 * ${old_brightness} / ${max_brightness} ))"

new_brightness_p="$(( ${old_brightness_p} $1 ))"

new_brightness="$(( ${max_brightness} * ${new_brightness_p} / 100 ))"

sudo chmod 666 "${basedir}brightness"
echo ${new_brightness} > "${basedir}brightness"
