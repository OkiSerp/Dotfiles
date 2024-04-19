if not status --is-interactive
  return
end

fish_config theme choose "CatpMocha"

set -U fish_greeting

bind \ek "history-search-backward"
bind \ej "history-search-forward"

bind \el "forward-char"

if test -d $HOME/.config/emacs/bin
  fish_add_path $HOME/.config/emacs/bin
end

set -gx DOOMDIR $HOME/.config/doom
alias emacs (which emacs)\ --no-window-system

if type -q nvim
  set -gx EDITOR (which nvim)
  alias e $EDITOR
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
  alias l "$(which lsd) --group-dirs first --icon never --color always \
  --blocks permission,user,size,date,git,name -lA --date \"+%y/%m/%d\""
  alias lt "$(which lsd) --group-dirs first --icon never --color always \
  --blocks permission,user,size,date,git,name -lAX --date \"+%y/%m/%d\" \
  --tree -I .git -I node_modules"
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
