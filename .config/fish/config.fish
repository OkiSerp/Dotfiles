if status is-interactive
  fish_config theme choose "CatpMocha"
end

set -g fish_greeting

fish_vi_key_bindings

bind -M default \ek "history-search-backward"
bind -M default \ej "history-search-forward"

bind -M insert \ek "history-search-backward"
bind -M insert \ej "history-search-forward"

bind -M insert \el "forward-char"

set -gx EDITOR (which nvim)

source "$HOME/.config/fish/aliases.fish"

fish_add_path $HOME/.config/emacs/bin
set -gx DOOMDIR $HOME/.config/doom

if not functions -q fundle; eval (curl -sfL https://git.io/fundle-install); end

fundle plugin "IlanCosman/tide@v6"
fundle plugin "PatrickF1/fzf.fish"

fundle init

set -gx FZF_DEFAULT_OPTS "--bind=alt-j:down,alt-k:up \
--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
--color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"

set fzf_fd_opts --hidden --no-ignore --exclude=.git --exclude=node_modules

if [ "$fish_key_bindings" = fish_vi_key_bindings ]
  bind -Minsert ! __history_previous_command
  bind -Minsert '$' __history_previous_command_arguments
else
  bind ! __history_previous_command
  bind '$' __history_previous_command_arguments
end

function __history_previous_command
  switch (commandline -t)
    case "!"
      commandline -t $history[1]
      commandline -f repaint
    case "*"
      commandline -i !
  end
end

function __history_previous_command_arguments
  switch (commandline -t)
    case "!"
      commandline -t ""
      commandline -f history-token-search-backward
    case "*"
      commandline -i '$'
  end
end

zoxide init fish | source
alias cd "__zoxide_z"
