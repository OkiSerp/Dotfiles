#!/usr/bin/env sh

deps="base-devel git libx11 libxft libxinerama xorg-server xorg-xinit terminus-font"
if [[ -x "$(command -v yay)" ]]; then
  yay -S --needed ${deps}
else
  sudo pacman -S --needed ${deps}
fi

if [[ ! -d "${HOME}/.suckless" ]]; then
  mkdir ${HOME}/.suckless
fi

tools=( "dwm" "dmenu" "st" )
for tool in ${tools[@]}; do
  if [[ ! -d "${HOME}/.suckless/${tool}" ]]; then
    git clone https://git.suckless.org/${tool} ${HOME}/.suckless/${tool}
    cd ${HOME}/.suckless/${tool}
    make && sudo make clean install
  fi
done

if [[ ! -d "/usr/share/xsessions" ]]; then
  sudo mkdir /usr/share/xsessions
fi

cat > ${HOME}/dwm.desktop << EOF
[Desktop Entry]
Type=XSession
Exec=$(which dwm)
TryExec=$(which dwm)
Comment=Dynamic Window Manager
Name=DWM
EOF

if [[ ! -f "/usr/share/xsessions/dwm.desktop" ]]; then
  sudo mv ${HOME}/dwm.desktop /usr/share/xsessions
else
  rm ${HOME}/dwm.desktop
fi

cd ${HOME}
