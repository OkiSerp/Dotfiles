[[ $- != *i* ]] && return

[[ $DISPLAY ]] && shopt -s checkwinsize

export HISTCONTROL=ignoreboth

export HISTSIZE=2000
export HISTFILESIZE=2000

shopt -s histappend

shopt -s autocd

bind "set completion-ignore-case on"

if [[ -x "$(command -v nvim)" ]]; then
  alias e="$(which nvim)"
fi

if [[ -x "$(command -v lsd)" ]]; then
  alias l="$(which lsd) --group-dirs first --icon never --color always \
    --blocks permission,user,size,date,git,name -lA --date \"+%y/%m/%d\""
fi

if [[ -x "$(command -v bat)" ]]; then
  alias cat="$(which bat) -p"
fi

alias mv="$(which mv) -i"
alias cp="$(which cp) -i"

alias du="$(which du) -h"
alias df="$(which df) -h"
alias free="$(which free) -m"

alias grep="$(which grep) --color=always"
alias egrep="$(which grep) --color=always -E"

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
