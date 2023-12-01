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
