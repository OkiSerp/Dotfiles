#!/usr/bin/env fish

#  ________  ___       __   _____ ______
# |\   ___ \|\  \     |\  \|\   _ \  _   \
# \ \  \_|\ \ \  \    \ \  \ \  \\\__\ \  \
#  \ \  \ \\ \ \  \  __\ \  \ \  \\|__| \  \
#   \ \  \_\\ \ \  \|\__\_\  \ \  \    \ \  \
#    \ \_______\ \____________\ \__\    \ \__\
#     \|_______|\|____________|\|__|     \|__|

#
# DWM Installation Script
# by Oleksii Kapula
#

# Dependencies for DWM without xorg ones.
set -l deps \
  "base-devel" "git" \
  "libx11" "libxft" "libxinerama" \
  "feh" "brightnessctl" "sxhkd" "xbanish" \
  "pipewire" "pipewire-alsa" "pipewire-pulse" "pipewire-jack" \
  "wireplumber" "alsa-utils"

# Xorg dependencies without its prefix.
set -l xdeps "server" "xinit" "xprop" "xsetroot" "xset" "xinput"

# Append xorg dependencies with its prefix to all dependencies.
for dep in $xdeps
  set -a deps (string join "" "xorg-" $dep)
end

# Check if yay package manager installed if not install it.
if not type -q yay
  sudo pacman --sync --needed base-devel git
  git clone https://aur.archlinux.org/yay-bin.git
  cd yay-bin/ && makepkg -si
  cd - && rm yay-bin/ -rf
end

# Install all necessary dependencies.
yay --sync --needed $deps

# Define suckless directory.
set -l sldir $HOME/.suckless

# If suckless directory doesn't exist then make it.
if not test -d $sldir
  mkdir -p $sldir
end

# Set suckless tools for installation.
set -l tools "dwm" "dmenu" "st"

# Install suckless tools if such don't appear in suckless directory.
for tool in $tools
  if test -d $sldir/$tool; continue; end
  git clone https://git.suckless.org/$tool $sldir/$tool
  cd $sldir/$tool
  make && sudo make clean install
  cd -
end

# Check if xsession directory exists if not then make it.
if not test -d /usr/bin/xsessions
  sudo mkdir /usr/bin/xsessions
end

# Create DWM xsession file
printf "%s\n%s\n%s\n%s\n%s" \
  "[Desktop Entry]" \
  "Type=XSession" \
  "Exec=$(which dwm)" \
  "Comment=Dynamic Window Manager" \
  "Name=DWM" > $HOME/dwm.desktop

# Move DWM xsession file to xsession directory.
if test -f /usr/share/xsessions/dwm.desktop
  rm $HOME/dwm.desktop
else
  sudo mv $HOME/dwm.desktop /usr/share/xsessions
end
