export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"

export HISTFILE="${XDG_STATE_HOME}/bash/history"

export LESSHISTFILE="/dev/null"

if [[ -f "$HOME/.bashrc" ]]; then
    source "$HOME/.bashrc"
fi

if [[ -d "$HOME/.local/bin" ]]; then
    export PATH="$HOME/.local/bin:$PATH"
fi

if [[ -x "$(command -v nvim)" ]]; then
  EDITOR="$(which nvim)"
  export EDITOR
fi

if [[ -d "$HOME/.config/emacs/bin" ]]; then
  export PATH="$HOME/.config/emacs/bin:$PATH"
  export DOOMDIR="$HOME/.config/doom"
fi
