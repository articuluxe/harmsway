# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

#echo "Loading .bashrc..."
set -o emacs
shopt -s cdspell
if [[ $BASH_VERSINFO -ge 4 ]]; then
    shopt -s dirspell
fi
# verify commands before running abbrev.
shopt -s histverify
# append to history from multiple sessions
shopt -s histappend
# ensure core dump on SEGV
ulimit -c unlimited
source ~/config/git-completion.sh
source ~/config/git-prompt.sh

alias pscores='ps -eL -o user,pid,tid,psr,comm,args'
alias magit='ex -e "(magit-status \"$(pwd)\")" > /dev/null 2>&1'
alias bt='echo 0 | gdb -batch-silent -ex "run" -ex "set logging overwrite on" -ex "set logging file gdb.bt" \
      -ex "set logging on" -ex "set pagination off" -ex "handle SIG33 pass nostop noprint" \
      -ex "echo backtrace:\n" -ex "backtrace full" -ex "echo \n\nregisters:\n" -ex "info registers" \
      -ex "echo \n\ncurrent instructions:\n" -ex "x/16i \$pc" -ex "echo \n\nthreads backtrace:\n" \
      -ex "thread apply all backtrace" -ex "set logging off" -ex "quit" --args'

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

    PS1+="${red}\h:${rst}"
    if [ ! -z "$CONDA_DEFAULT_ENV" ] && [ "$CONDA_DEFAULT_ENV" != "base" ]; then
        PS1+=" ${green}($(basename $CONDA_DEFAULT_ENV)) ${rst}"
    fi
    PS1+="${yellow}\w${rst}${purple}\$(__git_ps1 \" (%s)\")${rst}"
    if [ $EXIT != 0 ]; then
        PS1+=" ${redb}[${EXIT}]${rst}${blue}>${rst} "
    else
        PS1+="${blue}>${rst} "
    fi
}

# marks
export MARKPATH=$HOME/.marks
function mark {
    mkdir -p "$MARKPATH"; ln -s "$(pwd)" "$MARKPATH/$1"
}
function unmark {
    rm -i "$MARKPATH/$1"
}
function marks {
    ls -l "$MARKPATH" | sed 's/  / /g' | cut -d' ' -f9- | sed 's/ -/\'$'\t-/g' && echo
    # put \'$' before \t to make non-gnu sed understand tabs and newlines
}
function jump {
    cd -P "$MARKPATH/$1" 2>/dev/null || echo "No such mark: $1"
}
_completemarks() {
    local exe
    exe="$(which gfind)"
    test -z "$exe" && exe="find"
    local curw=${COMP_WORDS[COMP_CWORD]}
    local wordlist
    wordlist=$("$exe" "$MARKPATH" -type l -printf "%f\\n")
    COMPREPLY=($(compgen -W "${wordlist[@]}" -- "$curw"))
    return 0
}
complete -F _completemarks jump unmark
# alternate
export CDPATH=.:~/.marks/
function bookmark {
    ln -sr "$(pwd)" ~/.marks/"$1"
}
# end marks

# git helpers
function ahead_behind {
    curr_branch=$(git rev-parse --abbrev-ref HEAD);
    curr_remote=$(git config branch.$curr_branch.remote);
    curr_merge_branch=$(git config branch.$curr_branch.merge | cut -d / -f 3);
    git rev-list --left-right --count $curr_branch...$curr_remote/$curr_merge_branch | tr -s '\t' '|';
}

function git-list-branches {
    git for-each-ref --format='%(authorname) %09 %(refname)'
}

# pip bash completion start
_pip_completion()
{
    COMPREPLY=( $( COMP_WORDS="${COMP_WORDS[*]}" \
                   COMP_CWORD=$COMP_CWORD \
                   PIP_AUTO_COMPLETE=1 $1 ) )
}
complete -o default -F _pip_completion pip
# pip bash completion end

user=$(id -nu)
os=$(uname)
# use $HOSTNAME if available (which works on windows);
# if a FQDN, just take the device.
domain=$(echo "$HOSTNAME" | sed -e 's/\..*$//')
host=${domain:-$(hostname -s)}

# load any user settings
[ -r ~/."$user".env ] && . ~/."$user".env
[ -r ~/.bash_"$user" ] && . ~/.bash_"$user"

# Load any os settings
[ -r ~/."$os".env ] && . ~/."$os".env
[ -r ~/.bash_"$os" ] && . ~/.bash_"$os"

# Load any local settings
[ -r ~/."$host".env ] && . ~/."$host".env
[ -r ~/.bash_"$host" ] && . ~/.bash_"$host"

# If we are in a container, initialize it
[ -r ~/.singularity_profile ] && test "${SINGULARITY_NAME}" && . ~/.singularity_profile

# Source personal data
[ -r ~/.personal.env ] && . ~/.personal.env

[ -r ~/.emacs_bash ] && . ~/.emacs_bash
