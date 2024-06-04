set -x XDG_DATA_HOME $HOME/.local/share
set -x XDG_STATE_HOME $HOME/.local/state
set -x XDG_CACHE_HOME $HOME/.cache
set -x XDG_CONFIG_HOME $HOME/.config

set -x PASSWORD_STORE_DIR $XDG_DATA_HOME/pass

set -x MPLAYER_HOME $XDG_CONFIG_HOME/mplayer
set -x GTK2_RC_FILES $XDG_CONFIG_HOME/gtk-2.0/gtkrc
set -x W3M_DIR $XDG_DATA_HOME/w3m

test -d $HOME/.local/bin && fish_add_path $HOME/.local/bin

if test -d $HOME/.config/emacs/bin
  fish_add_path $HOME/.config/emacs/bin
  set -x DOOMDIR $HOME/.config/doom
end

if type -q nvim
  set -x EDITOR (which nvim)
  set -x MANPAGER (which nvim)\ +Man!
end

set -x nvm_default_version lts/iron
set -x nvm_default_packages yarn pnpm

if not functions -q fundle
  eval (curl -sfL https://git.io/fundle-install)
end

fundle plugin "jorgebucaran/nvm.fish"

fundle plugin "PatrickF1/fzf.fish"

fundle init

set -x FZF_DEFAULT_OPTS "--bind=alt-j:down,alt-k:up \
  --color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
  --color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
  --color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"

set fzf_fd_opts --hidden --no-ignore --exclude=.git --exclude=node_modules

if not status --is-interactive; return; end

set -U fish_greeting

fish_config theme choose "CatpMocha"

if set -q KDE_FULL_SESSION
  alias logout "qdbus6 org.kde.Shutdown /Shutdown logout"
end

type -q zoxide && zoxide init fish | source

functions -q __zoxide_z && alias cd __zoxide_z

type -q nvim && alias e (which nvim)

type -q emacs && alias emacs (which emacs)\ -nw

alias ip (which ip)\ -color=always

alias mv (which mv)\ -i
alias cp (which cp)\ -i

alias du (which du)\ -h
alias df (which df)\ -h
alias free (which free)\ -m

alias cal (which cal)\ -m

alias pwd (which pwd)\ -P

if type -q bat; alias cat (which bat); end

if type -q lsd
  alias l "$(which lsd) --group-dirs first --icon never --color always \
    --blocks permission,size,date,name -lA --date \"+%y/%m/%d\""
end

type -q wget && alias wget (which wget)\ --no-hsts

bind -M default \ek "history-search-backward"
bind -M default \ej "history-search-forward"

bind -M default \el "forward-char"
bind -M default \eF "forward-bigword"
bind -M default \eB "backward-bigword"

bind -M default \ea "kill-whole-line"
bind -M default \eD "backward-kill-word"

bind -M default "!" __history_previous_command
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
