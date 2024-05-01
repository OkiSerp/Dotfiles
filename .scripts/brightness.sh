#!/usr/bin/env sh

basedir="/sys/class/backlight/$(ls /sys/class/backlight/)/"

old_brightness="$(cat ${basedir}'brightness')"

max_brightness="$(cat ${basedir}'max_brightness')"

# get current brightness %
old_brightness_p="$(( 100 * ${old_brightness} / ${max_brightness} ))"
echo ${old_brightness_p}

# calculate new brightness %
new_brightness_p="$(( ${old_brightness_p} $1 ))"

# calculate new brightness value
new_brightness="$(( ${max_brightness} * ${new_brightness_p} / 100 ))"

# set the new brightness value
sudo chmod 666 "${basedir}brightness"
echo ${new_brightness} > "${basedir}brightness"
