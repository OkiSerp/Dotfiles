alias vi (which nvim)
alias vim (which nvim)

alias mv "$(which mv) -i"
alias cp "$(which cp) -i"

alias du "$(which du) -h"
alias df "$(which df) -h"
alias free "$(which free) -m"

alias cls "$(which clear)"

alias neofetch "$(which echo) && $(which neofetch)"

alias ls "$(which lsd) --group-dirs first --icon never --color always"
alias la "$(which lsd) --group-dirs first --icon never --color always -A"

alias ll "$(which lsd) --group-dirs first --icon never --icon-theme fancy \
--color always --blocks permission,user,size,date,git,name -lA --date \"+%y/%m/%d\""

alias lt "$(which lsd) --group-dirs first --icon never --icon-theme fancy \
--color always --blocks permission,user,size,date,git,name -lAX \
--date \"+%y/%m/%d\" --tree -I .git -I node_modules"
