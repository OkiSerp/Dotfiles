#!/usr/bin/env bash

source "${HOME}/.config/bash/variables"

[[ ! -t 0 ]] && return

[[ "${DISPLAY}" ]] && shopt -s checkwinsize

source "${HOME}/.config/bash/prompt"

shopt -s autocd

bind '"\ek": previous-history'
bind '"\ej": next-history'
bind '"\ea": kill-whole-line'

bind "set completion-ignore-case on"

if [[ -r "/usr/share/doc/pkgfile/command-not-found.bash" ]]; then
  source "/usr/share/doc/pkgfile/command-not-found.bash"
fi

if [[ -r "/usr/share/bash_completion/bash_completion" ]]; then
  source "/usr/share/bash_completion/bash_completion"
fi

if [[ -x "$(command -v dircolors)" ]]; then
  test -r "${HOME}/.dircolors" || dircolors -p > "${HOME}/.dircolors"
  eval "$(dircolors -b "${HOME}/.dircolors")"
fi

source "${HOME}/.config/bash/aliases"

if [[ -x "$(command -v zoxide)" ]];then
  eval "$(zoxide init bash)"
  alias cd="z"
fi
