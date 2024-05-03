export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"

export PASSWORD_STORE_DIR="$XDG_DATA_HOME/pass"

export HISTFILE="${XDG_STATE_HOME}/bash/history"

export LESSHISTFILE="/dev/null"

[[ -x "$(command -v nvim)" ]] && export EDITOR="$(which nvim)"

[[ $- != *i* ]] && return

PS1="[\u@\h \W]\$ "

shopt -s autocd

bind "set completion-ignore-case on"

alias mv="$(which mv) -i"
alias cp="$(which cp) -i"

alias du="$(which du) -h"
alias df="$(which df) -h"
alias free="$(which free) -m"

alias grep="$(which grep) --color=auto"
alias egrep="$(which grep) --color=auto -E"

alias ip="$(which ip) -color=auto"

alias l="$(which ls) -gGAh --group-directories-first --color=auto"


[[ -x "$(command -v nvim)" ]] && alias e="$(which nvim)"

if [[ -f /usr/share/doc/pkgfile/command-not-found.bash ]]; then
  source /usr/share/doc/pkgfile/command-not-found.bash
fi

if [[ -f /usr/share/bash_completion/bash_completion ]]; then
  source /usr/share/bash_completion/bash_completion
fi
