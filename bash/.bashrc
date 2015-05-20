# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

#echo "Loading .bashrc..."

# User specific aliases and functions
gcc=/usr/local/gcc-4.5.1/bin
python=/opt/anaconda-python-2.7.8/bin
export PATH=.:$HOME/bin:/sbin/:/usr/lib64:$PATH
export ORIGPATH=$PATH
#:$python:$gcc:
export MANPATH=
export NAME='Dan Harms'
export DISPLAY=10.10.102.157:0.0

# verify commands before running abbrev.
shopt -s histverify
source ~/config/git-completion.sh
source ~/config/git-prompt.sh
export PS1="\[\e[31m\]\h\[\e[m\]\[\e[31m\]:\[\e[m\]\[\e[33m\]\w\[\e[m\]\[\e[31m\]\$(__git_ps1 \" (%s)\")\[\e[m\]> "

# alias git-current-branch='git branch 2>&1 | grep '\''*'\'' | perl -pe '\''s/^\* (.*)/ \(\1\)/'\'''
# export PS1="\[\e[31m\]\h\[\e[m\]\[\e[31m\]:\[\e[m\]\[\e[33m\]\w\[\e[m\]\[\e[31m\]$(git-current-branch)\[\e[m\]> "


export CSTDLIB_ROOT=/usr/local/gcc-4.5.1/include/c++/4.5.1

# SNAP
alias emacs='emacs -nw'
alias edit='emacs'

snap-setup() { export SNAP_ROOT=`pwd` && source .snap-env; }
snap-init()
{
   if [ -z "$1" ]; then
      echo "Missing snap version; exiting..."
      exit
   fi
   echo "Using snap version $1"
   cp ~/config/env/snap/"$1"/.env ./.snap-env
}

gcc-init()
{
   if [ -z "$1" ]; then
      echo "Missing gcc version; exiting..."
      exit
   fi
   echo "Using gcc $2"
   cp ~/config/env/gcc/"$1"/.env ./.gcc-env
}

export ONE_TICK_CONFIG=/opt/1tick/client_data/config/one_tick_config.txt

#export SNAP_STATIC_TRUNK=~/src/curr/snap-static
#export LD_LIBRARY_PATH=/usr/local/snap/lib:$LD_LIBRARY_PATH
# use gcc451

# ensure core dump on SEGV
ulimit -c unlimited
