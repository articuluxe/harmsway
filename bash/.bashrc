# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

#echo "Loading .bashrc..."

function ahead_behind {
    curr_branch=$(git rev-parse --abbrev-ref HEAD);
    curr_remote=$(git config branch.$curr_branch.remote);
    curr_merge_branch=$(git config branch.$curr_branch.merge | cut -d / -f 3);
    git rev-list --left-right --count $curr_branch...$curr_remote/$curr_merge_branch | tr -s '\t' '|';
}

function git-list-branches {
    git for-each-ref --format='%(authorname) %09 %(refname)'
}

set -o emacs
# auto-expand !-key-chords on <space>
bind Space:magic-space
# verify commands before running abbrev.
shopt -s histverify
# ensure core dump on SEGV
ulimit -c unlimited
source ~/config/git-completion.sh
source ~/config/git-prompt.sh

export PROMPT_COMMAND=__prompt_command
function __prompt_command() {
    local EXIT="$?"             # needs to be first
    PS1=""
    local rst='\[\e[0m\]'
    local red='\[\e[0;31m\]'
    local redb='\[\e[1;31m\]'
    local green='\[\e[0;32m\]'
    local yellow='\[\e[0;33m\]'
    local blue='\[\e[0;34m\]'
    local purple='\[\e[0;35m\]'

    PS1+="${red}\h:${rst}${yellow}\w${rst}${purple}\$(__git_ps1 \" (%s)\")${rst}"
    if [ $EXIT != 0 ]; then
        PS1+=" ${redb}[${EXIT}]${rst}${blue}>${rst} "
    else
        PS1+="${blue}>${rst} "
    fi
}

os=$(uname)
# use $HOSTNAME if available (which works on windows);
# if a FQDN, just take the device.
domain=$(echo $HOSTNAME | sed -e 's/\..*$//')
host=${domain:-$(hostname -s)}

# Load any os settings
if [ -f ~/.bash_$os ]; then
    . ~/.bash_$os
fi

# Load any local settings
if [ -f ~/.bash_$host ]; then
    . ~/.bash_$host
fi

# Source personal data
if [ -f ~/.personal ]; then
    . ~/.personal
fi

if [ -f ~/.emacs_bash ]; then
    . ~/.emacs_bash
fi
