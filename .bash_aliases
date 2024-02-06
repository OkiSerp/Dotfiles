if [[ -x "$(command -v lsd)" ]]; then
    alias ls="$(which lsd) --group-dirs first --icon never --color always"
    alias la="$(which lsd) --group-dirs first --icon never --color always -A"

    alias ll="$(which lsd) --group-dirs first --icon never --icon-theme fancy --color always \
        --blocks permission,user,size,date,git,name -lA --date \"+%y/%m/%d\""

    alias lt="$(which lsd) --group-dirs first --icon never --icon-theme fancy --color always \
        --blocks permission,user,size,date,git,name -lAX --date \"+%y/%m/%d\" \
        --tree -I .git -I node_modules"
else
    alias ls="$(which ls) --group-directories-first --color=always"
    alias la="$(which ls) -A --group-directories-first --color=always"

    alias ll="$(which ls) -lAhGX --group-directories-first \
        --time-style='+%y/%m/%d' --color=always"

    alias lt="$(which ls) -lAhGXR -I .git --group-directories-first \
        --time-style='+%y/%m/%d' --color=always"
fi

alias grep="$(which grep) --color=always"

alias head="$(which sed) 11q"

alias mv="$(which mv) -i"
alias cp="$(which cp) -i"

alias du="$(which du) -h"
alias df="$(which df) -h"
alias free="$(which free) -m"

alias cls="$(which clear)"

alias vi="$(which nvim)"
alias vim="$(which nvim)"

alias neofetch="$(which echo) && $(which neofetch)"
