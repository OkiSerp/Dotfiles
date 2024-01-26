[[ $- != *i* ]] && return

[[ $DISPLAY ]] && shopt -s checkwinsize

export HISTCONTROL=ignoreboth

export HISTSIZE=2000
export HISTFILESIZE=2000

shopt -s histappend

shopt -s autocd

export LESSHISTFILE="/dev/null"

export EDITOR="/usr/bin/nvim"
export VISUAL="/usr/bin/nvim"

if [ -f $HOME/.bash_aliases ]; then
    source $HOME/.bash_aliases
fi

if [ -d "$HOME/.config/emacs/bin" ] ; then
    export PATH="$HOME/.config/emacs/bin:$PATH"
fi

export DOOMDIR="$HOME/.config/doom"

set -o vi

bind -m vi-insert "\"\e-k\":previous-history"
bind -m vi-insert "\"\e-j\":next-history"
bind -m vi-insert "\"\C-l\":clear-screen"

bind "set completion-ignore-case on"
bind "set show-mode-in-prompt off"

RST="\\[\\033[00m\\]"
RED="${RST}\\[\\033[01;31m\\]"
GRN="${RST}\\[\\033[01;32m\\]"
YLW="${RST}\\[\\033[03;33m\\]"
BLU="${RST}\\[\\033[01;34m\\]"
PUR="${RST}\\[\\033[01;35m\\]"

PGB="\$(git branch 2> /dev/null | sed -e \
    '/^[^*]/d' -e 's/* \(.*\)/ ${BLU}⌈${GRN}\1${BLU}⌋/')"

export PS1="${BLU}⌊${PUR}\u${RED}@${BLU}\h ${YLW}\W${RST}${BLU}⌉${PGB}${RED} ✘ ${RST}"
