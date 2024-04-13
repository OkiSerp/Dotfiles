if not status --is-interactive
  return
end

fish_config theme choose "CatpMocha"

set -U fish_greeting

bind \ek "history-search-backward"
bind \ej "history-search-forward"

bind \el "forward-char"

fish_add_path $HOME/.config/emacs/bin
set -gx DOOMDIR $HOME/.config/doom

if type -q nvim
  set -gx EDITOR (which nvim)
  alias vi $EDITOR
  alias vim $EDITOR
end

alias mv (which mv)\ -i
alias cp (which cp)\ -i

alias du (which du)\ -h
alias df (which df)\ -h
alias free (which free)\ -m

alias grep "$(which grep) --color=always"
alias egrep "$(which grep) --color=always -E"

if type -q bat
  alias cat (which bat)\ -p
end

alias cls (which clear)

if type -q neofetch
  alias neofetch "$(which echo) && $(which neofetch)"
end

if type -q lsd
  alias ls "$(which lsd) --group-dirs first --icon never --color always"
  alias la "$(which lsd) --group-dirs first --icon never --color always -A"
  alias ll "$(which lsd) --group-dirs first --icon never --color always \
  --blocks permission,user,size,date,git,name -lA --date \"+%y/%m/%d\""
  alias lt "$(which lsd) --group-dirs first --icon never --color always \
  --blocks permission,user,size,date,git,name -lAX --date \"+%y/%m/%d\" \
  --tree -I .git -I node_modules"
else
  alias ls "$(which ls) --group-directories-first --color=always"
  alias la "$(which ls) -AX --group-directories-first --color=always"
  alias ll "$(which ls) -oAhX --group-directories-first --color=always \
  --indicator-style=none --time-style=\"+%y/%m/%d\""
end

if type -q zoxide
  zoxide init fish | source
  alias cd "__zoxide_z"
end

set -gx nvm_default_version lts/iron
set -gx nvm_default_packages yarn pnpm bun

if not functions -q fundle; eval (curl -sfL https://git.io/fundle-install); end

fundle plugin "jorgebucaran/nvm.fish"

fundle init

bind ! __history_previous_command
bind "\$" __history_previous_command_arguments

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
