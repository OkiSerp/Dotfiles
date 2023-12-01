if status is-interactive
  fish_config theme choose "Catppuccin Mocha"
end

set -g fish_greeting

fish_vi_key_bindings

bind -M insert \el "forward-char"
bind -M insert \ek "history-search-backward"
bind -M insert \ej "history-search-forward"

bind -M default \ek "history-search-backward"
bind -M default \ej "history-search-forward"

alias vi  (which nvim)
set -gx EDITOR (which nvim)

fish_add_path $HOME/.config/emacs/bin
set -gx DOOMDIR $HOME/.config/doom

fish_add_path $HOME/.local/bin

alias mv "$(which mv) -i"
alias cp "$(which cp) -i"

alias neofetch "echo && $(which neofetch)"

alias ls "lsd --group-dirs first --icon never --color always"
alias la "lsd --group-dirs first --icon never --color always -A"
alias ll "lsd --group-dirs first --icon always --icon-theme fancy --color always \
  --blocks permission,user,size,git,name -lA"
alias lt "lsd --group-dirs first --icon always --icon-theme fancy --color always \
  --blocks permission,user,size,git,name -lAX --tree -I .git -I node_modules"
