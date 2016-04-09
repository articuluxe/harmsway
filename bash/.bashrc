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

os=$(uname)
host=$(hostname -s)

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

[ "${INSIDE_EMACS}" != "" ] && export TERM=emacs
