if [[ -f "$HOME/.bashrc" ]]; then
  source "$HOME/.bashrc"
fi

if [[ -d "$HOME/.local/bin" ]]; then
  export PATH="$HOME/.local/bin:$PATH"
fi

if [[ -d "$HOME/.config/emacs/bin" ]]; then
  export PATH="$HOME/.config/emacs/bin:$PATH"
  export DOOMDIR="$HOME/.config/doom"
fi
