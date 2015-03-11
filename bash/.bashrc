# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions
alias git-current-branch='git branch 2>&1 | grep '\''*'\'' | perl -pe '\''s/^\* (.*)/ \(\1\)/'\'''

export PS1="\[\e[31m\]\h\[\e[m\]\[\e[31m\]:\[\e[m\]\[\e[33m\]\w\[\e[m\]\[\e[31m\]$(git-current-branch)\[\e[m\]> "

export ONE_TICK_CONFIG=/opt/1tick/client_data/config/one_tick_config.txt
