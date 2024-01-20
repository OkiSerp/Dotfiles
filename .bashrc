[[ $- != *i* ]] && return

HISTTIMEFORMAT="%F %T "
HISTCONTROL=ignoredups
HISTSIZE=2000
HISTFILESIZE=2000
shopt -s histappend

red='\[\033[01;31m\]'
grn='\[\033[01;32m\]'
ylw='\[\033[01;33m\]'
blu='\[\033[01;34m\]'
pur='\[\033[01;35m\]'
wht='\[\033[01;37m\]'
clr='\[\033[00m\]'

pgb="\$(git branch 2> /dev/null | sed -e \
  '/^[^*]/d' -e 's/* \(.*\)/ $blu($wht\1$blu)/')"

export PS1="$blu[$pur\u$red@$grn\h $ylw\W$blu]$pgb $red\$$clr "

bind "set completion-ignore-case on"
bind "set show-mode-in-prompt on"

bind "set vi-ins-mode-string \
  \1\e[34;1m\2(\1\e[33;1m\2I\1\e[34;1m\2) \1\e[0m\2"

bind "set vi-cmd-mode-string \
  \1\e[34;1m\2(\1\e[37;1m\2N\1\e[34;1m\2) \1\e[0m\2"

if [[ "$INSIDE_EMACS" = "vterm" ]]; then
    set -o emacs
    bind "set show-mode-in-prompt off"
fi

export PATH="$HOME/.config/emacs/bin/:$PATH"
export DOOMDIR="$HOME/.config/doom/"

export EDITOR="$(which nvim)"

set -o vi

bind -m vi-command '"\C-k": previous-history'
bind -m vi-command '"\C-j": next-history'

bind -m vi-insert '"\C-k": previous-history'
bind -m vi-insert '"\C-j": next-history'

bind -m emacs '"\C-k": previous-history'
bind -m emacs '"\C-j": next-history'

bind -m vi-insert '"\C-l": clear-screen'

alias sourceb="source ~/.bashrc"
alias bconf="$EDITOR ~/.bashrc"

alias vi="$(which nvim)"
alias vim="$(which nvim)"

alias ..="cd .. && pwd"
alias ...="cd ../.. && pwd"

cd() {
  if [ "$1" == "-" ]; then
    builtin cd "$@" || pwd
  else
    builtin cd "$@" && pwd
  fi
}

alias mv="$(which mv) -i"
alias cp="$(which cp) -i"

alias du="$(which du) -h"
alias df="$(which df) -h"
alias free="$(which free) -h"

alias cal="$(which cal) -m"

alias egrep="grep -E"

alias ls="$(which lsd) --group-dirs first \
  --icon never --color always"

alias la="$(which lsd) --group-dirs first \
  --icon never --color always -A"

alias ll="$(which lsd) --group-dirs first --icon never \
  --icon-theme fancy --color always \
  --blocks permission,user,size,git,name -Al"

alias lt="$(which lsd) --group-dirs first --icon never \
  --icon-theme fancy --color always \
  --blocks permission,user,size,git,name \
  -X -Al --tree -I .git -I node_modules"

ld() {
  /usr/bin/lsd --group-dirs first --icon never \
    --icon-theme fancy --color always \
    --blocks permission,user,size,git,name \
    -X -Al "$@" | grep -E "^\."
}

ex () {
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1    ;;
      *.tar.gz)    tar xzf $1    ;;
      *.bz2)       bunzip2 $1    ;;
      *.rar)       unrar x $1    ;;
      *.gz)        gunzip $1     ;;
      *.tar)       tar xf $1     ;;
      *.tbz2)      tar xjf $1    ;;
      *.tgz)       tar xzf $1    ;;
      *.zip)       unzip $1      ;;
      *.Z)         uncompress $1 ;;
      *.7z)        7z x $1       ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}
