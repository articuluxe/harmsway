# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

#echo "Loading .bashrc..."

# verify commands before running abbrev.
shopt -s histverify
# ensure core dump on SEGV
ulimit -c unlimited
source ~/config/git-completion.sh
source ~/config/git-prompt.sh
export PS1="\[\e[31m\]\h\[\e[m\]\[\e[31m\]:\[\e[m\]\[\e[33m\]\w\[\e[m\]\[\e[31m\]\$(__git_ps1 \" (%s)\")\[\e[m\]> "
alias emacs='emacs -nw'

# Load any local settings
if [ -f ~/.bash_local ]; then
	. ~/.bash_local
fi

#export LD_LIBRARY_PATH=/usr/local/snap/lib:$LD_LIBRARY_PATH
# use gcc451
