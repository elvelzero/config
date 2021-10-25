
### DELETE FROM HERE ###
#
# /etc/bash.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[[ $DISPLAY ]] && shopt -s checkwinsize

### THE ONLY THING U NEED ###

# source $XDG_CONFIG_HOME/bash/bashrc | default: [[ -f ~/.bashrc ]] && . ~/.bashrc
# [[ -f ~/.config/bash/bashrc ]] && . ~/.config/bash/bashrc
# [[ -f $HOME/.config/bash/bashrc ]] && . $HOME/.config/bash/bashrc
[[ -f $XDG_CONFIG_HOME/bash/bashrc ]] && . $XDG_CONFIG_HOME/bash/bashrc

### THE ONLY THING U NEED ###

case ${TERM} in
  xterm*|rxvt*|Eterm|aterm|kterm|gnome*)
    PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033]0;%s@%s:%s\007" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/\~}"'

    ;;
  screen*)
    PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033_%s@%s:%s\033\\" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/\~}"'
    ;;
esac

[ -r /usr/share/bash-completion/bash_completion   ] && . /usr/share/bash-completion/bash_completion

### DELETE TILL HERE ###
