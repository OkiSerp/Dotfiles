if test -d $HOME/.local/bin
  fish_add_path $HOME/.local/bin
end

set -gx nvm_default_version lts/iron
set -gx nvm_default_packages yarn pnpm bun

if not functions -q fundle
  eval (curl -sfL https://git.io/fundle-install)
end

fundle plugin "jorgebucaran/nvm.fish"

fundle init

if test -d $HOME/.config/emacs/bin
  fish_add_path $HOME/.config/emacs/bin
  set -gx DOOMDIR $HOME/.config/doom
end

if type -q nvim; set -gx EDITOR (which nvim); end

if not status --is-interactive; return; end

set -U fish_greeting

fish_config theme choose "CatpMocha"

if type -q zoxide; zoxide init fish | source; end

if functions -q __zoxide_z
  alias cd "__zoxide_z"
end

if type -q nvim; alias e (which nvim); end

alias mv (which mv)\ -i
alias cp (which cp)\ -i

alias du (which du)\ -h
alias df (which df)\ -h
alias free (which free)\ -m

if type -q bat
  alias cat (which bat)\ -p
end

if type -q lsd
  alias l "$(which lsd) --group-dirs first --icon never --color always \
    --blocks permission,user,size,date,git,name -lA --date \"+%y/%m/%d\""
end

bind -M default \ek "history-search-backward"
bind -M default \ej "history-search-forward"

bind -M default \el "forward-char"

bind -M default ! __history_previous_command
bind -M default "\$" __history_previous_command_arguments

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
      commandline -i "\$"
  end
end
