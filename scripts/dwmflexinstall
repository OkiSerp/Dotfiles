#!/usr/bin/env fish

set -l dwmflexdir $HOME/.suckless/dmwflexipatch
set -l flexfinaldir $HOME/.suckless/flexfinalizer
set -l destdir $HOME/.suckless/dwm

git clone https://github.com/bakkeby/dwm-flexipatch.git $dwmflexdir

set -l patches \
  "VIEWONTAG" "PERTAG" "AUTOSTART" \
  "ATTACHBOTTOM" "ALWAYSCENTER" "ALT_TAB" \
  "VANITYGAPS" "XRDB" "MOVESTACK" "RESTARTSIG"

set -l patchfile dwmflexdir/patches.def.h

for patch in $patches
  sed -i "s#define ${patch}_PATCH 0/s#define ${patch}_PATCH 1" $patchfile
end

cd $dwmflexdir
make && sudo make clean install
cd -

git clone https://github.com/bakkeby/flexipatch-finalizer.git $flexfinaldir
cd $flexfinaldir
./flexipatch-finalizer.sh -r -d $dwmflexdir -o $destdir

cd $destdir
make && sudo make clean install
