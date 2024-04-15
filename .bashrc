[[ $- != *i* ]] && return

[[ $DISPLAY ]] && shopt -s checkwinsize

export HISTCONTROL=ignoreboth

export HISTSIZE=2000
export HISTFILESIZE=2000

shopt -s histappend

shopt -s autocd

bind "set completion-ignore-case on"

export LESSHISTFILE="/dev/null"

export SUDO_EDITOR="$(which nvim)"
export EDITOR="$(which nvim)"
export VISUAL="$(which nvim)"

if [[ -d "$HOME/.config/emacs/bin" ]] ; then
    export PATH="$HOME/.config/emacs/bin:$PATH"
fi

export DOOMDIR="$HOME/.config/doom"

if [[ -x "$(command -v lsd)" ]]; then
  alias ls="$(which lsd) --group-dirs first --icon never --color always"
  alias la="$(which lsd) --group-dirs first --icon never --color always -A"
  alias ll="$(which lsd) --group-dirs first --icon never --icon-theme fancy \
    --color always --blocks permission,user,size,date,git,name \
    -lA --date \"+%y/%m/%d\""
  alias lt="$(which lsd) --group-dirs first --icon never --icon-theme fancy \
    --color always --blocks permission,user,size,date,git,name -lAX \
    --date \"+%y/%m/%d\" --tree -I .git -I node_modules"
else
  alias ls="$(which ls) --group-directories-first --color=always"
  alias la="$(which ls) -AX --group-directories-first --color=always"
  alias ll="$(which ls) -oAhX --group-directories-first --color=always \
    --time-style='+%y/%m/%d'"
fi

alias vi="$(which nvim)"
alias vim="$(which nvim)"

alias neofetch="$(which echo) && $(which neofetch)"

alias grep="$(which grep) --color=always"
alias egrep="$(which grep) --color=always -E"

alias mv="$(which mv) -i"
alias cp="$(which cp) -i"

alias du="$(which du) -h"
alias df="$(which df) -h"
alias free="$(which free) -m"

alias cat="$(which bat) -p"

alias cls="$(which clear)"

RST="\\[\\033[00m\\]"
RED="${RST}\\[\\033[01;31m\\]"
GRN="${RST}\\[\\033[01;32m\\]"
YLW="${RST}\\[\\033[03;33m\\]"
BLU="${RST}\\[\\033[01;34m\\]"
PUR="${RST}\\[\\033[01;35m\\]"

PGB="\$(git branch 2> /dev/null | sed -e \
    '/^[^*]/d' -e 's/* \(.*\)/ ${BLU}(${GRN}\1${BLU})/')"

export PS1="${BLU}[${PUR}\u${RED}@${BLU}\h ${YLW}\W${RST}${BLU}]${PGB}${RED} » ${RST}"

eval "$(zoxide init bash)"
