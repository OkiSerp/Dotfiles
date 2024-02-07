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

alias vi (which nvim)
alias vim (which nvim)

set -gx EDITOR (which nvim)

fish_add_path $HOME/.config/emacs/bin
set -gx DOOMDIR $HOME/.config/doom

alias mv "$(which mv) -i"
alias cp "$(which cp) -i"

alias du="$(which du) -h"
alias df="$(which df) -h"
alias free="$(which free) -m"

alias cls="$(which clear)"

alias neofetch "$(which echo) && $(which neofetch)"

alias ls "$(which lsd) --group-dirs first --icon never --color always"
alias la "$(which lsd) --group-dirs first --icon never --color always -A"

alias ll "$(which lsd) --group-dirs first --icon never --icon-theme fancy --color always \
--blocks permission,user,size,date,git,name -lA --date \"+%y/%m/%d\" --header"

alias lt "$(which lsd) --group-dirs first --icon never --icon-theme fancy --color always \
--blocks permission,user,size,date,git,name -lAX --date \"+%y/%m/%d\" \
--tree -I .git -I node_modules"

if not functions -q fundle; eval (curl -sfL https://git.io/fundle-install); end

fundle plugin "IlanCosman/tide@v6"
fundle plugin "PatrickF1/fzf.fish"

fundle init

set -gx FZF_DEFAULT_OPTS --bind=alt-j:down,alt-k:up

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
