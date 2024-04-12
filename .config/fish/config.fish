if not status --is-interactive
  return
end

fish_config theme choose "CatpMocha"

set -g fish_greeting

bind \ek "history-search-backward"
bind \ej "history-search-forward"

bind \el "forward-char"

set -gx EDITOR (which nvim)

fish_add_path $HOME/.config/emacs/bin
set -gx DOOMDIR $HOME/.config/doom

zoxide init fish | source
alias cd "__zoxide_z"

alias vi (which nvim)
alias vim (which nvim)

alias mv (which mv)\ -i
alias cp (which cp)\ -i

alias du (which du)\ -h
alias df (which df)\ -h
alias free (which free)\ -m

alias cat (which bat)\ -p

alias cls (which clear)

alias neofetch "$(which echo) && $(which neofetch)"

if type -q lsd
  alias ls "$(which lsd) --group-dirs first --icon never --color always"
  alias la "$(which lsd) --group-dirs first --icon never --color always -A"
  alias ll "$(which lsd) --group-dirs first --icon never --icon-theme fancy \
  --color always --blocks permission,user,size,date,git,name \
  -lA --date \"+%y/%m/%d\""
  alias lt "$(which lsd) --group-dirs first --icon never --icon-theme fancy \
  --color always --blocks permission,user,size,date,git,name -lAX \
  --date \"+%y/%m/%d\" --tree -I .git -I node_modules"
else
  alias ls "$(which ls) --group-directories-first --color=always"
  alias la "$(which ls) -AX --group-directories-first --color=always"
  alias ll "$(which ls) -oAhX --group-directories-first --color=always \
  --indicator-style=none --time-style='+%y/%m/%d'"
end

bind ! __history_previous_command
bind '$' __history_previous_command_arguments

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
