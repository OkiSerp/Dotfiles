# Set XDG folders properly
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_CONFIG_HOME="${HOME}/.config"

# Move password store
export PASSWORD_STORE_DIR="${XDG_DATA_HOME}/pass"

# Stop trashing home directory
export XAUTHORITY="${XDG_RUNTIME_DIR}/Xauthority"
export GTK2_RC_FILES="${XDG_CONFIG_HOME}/gtk-2.0/gtkrc"
export MPLAYER_HOME="${XDG_CONFIG_HOME}/mplayer"
export W3M_DIR="${XDG_DATA_HOME}/w3m"

# Move history files for bash and less commands
export HISTFILE="${XDG_STATE_HOME}/bash/history"
export LESSHISTFILE="${XDG_STATE_HOME}/less/history"

# Add local programs to path
if [[ -d "${HOME}/.local/bin" ]]; then
  export PATH="${HOME}/.local/bin:$PATH"
fi

# Handle DOOM-Emacs
if [[ -d "${HOME}/.config/emacs/bin" ]]; then
  export PATH="${HOME}/.config/emacs/bin:$PATH"
  export DOOMDIR="${HOME}/.config/doom"
fi

# Default editor
if [[ -x `command -v nvim` ]];then
  export EDITOR=`which nvim`
  export MANPAGER="`which nvim` +Man!"
fi

# Setup fuzzy finder
export fzf_fd_opts="--hidden --no-ignore --exclude=.git --exclude=node_modules"
export FZF_DEFAULT_OPTS="--bind=alt-j:down,alt-k:up \
  --color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
  --color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
  --color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"

# If not running interactively, do not proceed
[[ ! -t 0 ]] && return

# Check window size
[[ "${DISPLAY}" ]] && shopt -s checkwinsize

# Colors for prompt
RST="\\[\\033[00m\\]"
RED="${RST}\\[\\033[00;31m\\]"
GRN="${RST}\\[\\033[00;32m\\]"
YLW="${RST}\\[\\033[00;33m\\]"
BLU="${RST}\\[\\033[00;34m\\]"
PUR="${RST}\\[\\033[00;35m\\]"

# Print git branch
PGB="\$(git branch 2> /dev/null | sed -e \
  '/^[^*]/d' -e 's/* \(.*\)/ ${BLU}(${GRN}\1${BLU})/')"

# Beautiful prompt
PS1="${BLU}${PUR}\u${RED}@${BLU}\h ${YLW}\W${RST}${BLU}${PGB}${RED} »› ${RST}"
export PS1

# Change directory without cd command
shopt -s autocd

# Make my life easier
bind '"\ek": previous-history'
bind '"\ej": next-history'
bind '"\ea": kill-whole-line'

# Ignore case when complete
bind "set completion-ignore-case on"

# Show how to find lacking command
if [[ -r /usr/share/doc/pkgfile/command-not-found.bash ]]; then
  source /usr/share/doc/pkgfile/command-not-found.bash
fi

# Enhanced completion
if [[ -r /usr/share/bash_completion/bash_completion ]]; then
  source /usr/share/bash_completion/bash_completion
fi

# Inform before overwrite
alias mv="`which mv` -i"
alias cp="`which cp` -i"

# Human readable
alias du="`which du` -h"
alias df="`which df` -h"
alias free="`which free` -m"

# Enable colors
alias grep="`which grep` --color=auto"
alias egrep="`which grep` --color=auto -E"
alias ip="`which ip` -color=auto"

# Monday first
alias cal="`which cal` -m"

# Show real path
alias pwd="`which pwd` -P"

# Is it catman? No, it's batman
test -x `command -v bat` && alias cat=`which bat`

# List dictories
if [[ -x `command -v lsd` ]]; then
  alias l="`which lsd` --group-dirs first --icon never --color always \
    --blocks permission,size,date,name -lA --date '+%y/%m/%d'"
else
  alias l="`which ls` -gGAh --group-directories-first --color=auto"
fi

# The must have editing alias
[[ -x `command -v nvim` ]] && alias e=`which nvim`

# No history for wget
[[ -x `command -v wget` ]] && alias wget="`which wget` --no-hsts"

# Change directories with lighting speed
if [[ -x `command -v zoxide` ]];then
  eval "$(zoxide init bash)"
  alias cd="z"
fi
