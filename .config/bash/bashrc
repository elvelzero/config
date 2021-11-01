#
# ~/.config/bash/bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# alias
alias ls='ls --color=auto'
alias ls='lsd'
alias cat='bat'
alias startx='startx "$XDG_CONFIG_HOME/X11/xinitrc"'
alias unimatrix='unimatrix -f -s 98 -c blue -l aAcCegGkmnopPrRsSu'

alias arduino-cli='arduino-cli --config-file $XDG_CONFIG_HOME/arduino15/arduino-cli.yaml'

# ~/.wgetrc .wget-hsts
alias wget='wget --hsts-file="$XDG_CACHE_HOME/wget-hsts"'
# set the hsts-file variable with an absolute path 
# as wgetrc does not support environment variables: 
# echo hsts-file \= "$XDG_CACHE_HOME"/wget-hsts >> "$XDG_CONFIG_HOME/wgetrc"

# PS1='[\u@\h \W]\$ '
export PS1="\[\e[31m\] \[\e[000m\]\[\e[33m\]\W \[\e[000m\]\[\e[32m\]\\$ \[\e[000m\]\[\e[34m\]> \[\e[000m\]"
export BAT_THEME="Nord"

# sdkman
export SDKMAN_DIR="$HOME/.local/bin/sdkman"
[[ -s "$HOME/.local/bin/sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.local/bin/sdkman/bin/sdkman-init.sh"

# JAVA_HOME
# export JAVA_HOME="~/.local/bin/jdk-17.0.1"
# export PATH="$JAVA_HOME/bin:$PATH"
