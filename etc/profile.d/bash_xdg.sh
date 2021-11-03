# Setting bash to use $XDG_CONFIG_HOME/bash, defaults to ~/.config/bash
confdir=${XDG_CONFIG_HOME:-$HOME/.config}/bash

# Make bash follow the XDG_CONFIG_HOME convention
_confdir=\${XDG_CONFIG_HOME:-\$HOME/.config}/bash
if [ -d "$_confdir" ] &&  [ "\$0" = "bash" ]
then
    . "\$_confdir"/bash_profile
    . "\$_confdir"/bashrc
fi
unset _confdir

if [ -s "\${XDG_CONFIG_HOME:-\$HOME/.config}/bash/bash_logout" ]
then
    . "\${XDG_CONFIG_HOME:-\$HOME/.config}/bash/bash_logout"
fi

# Setting zsh
## System wide configuration (using xdg directories)
if [[ -d "\${XDG_CONFIG_HOME:-\$HOME/.config}"/zsh ]]
then
        export ZDOTDIR=\${XDG_CONFIG_HOME:-\$HOME/.config}/zsh
fi
