# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

#echo "Loading .bashrc..."

# User specific aliases and functions
# gcc=/usr/local/gcc-4.5.1/bin
# python=/opt/anaconda-python-2.7.8/bin
export PATH=.:$HOME/bin:/sbin/:/usr/lib64:$PATH
export ORIGPATH=$PATH
export MANPATH=
export EDITOR='emacs -nw'
export EMACSX=emacs
export NAME='Dan Harms'
export DISPLAY=10.10.102.157:0.0

# verify commands before running abbrev.
shopt -s histverify
# ensure core dump on SEGV
ulimit -c unlimited
source ~/config/git-completion.sh
source ~/config/git-prompt.sh
export PS1="\[\e[31m\]\h\[\e[m\]\[\e[31m\]:\[\e[m\]\[\e[33m\]\w\[\e[m\]\[\e[31m\]\$(__git_ps1 \" (%s)\")\[\e[m\]> "
alias emacs='emacs -nw'
alias emacs-compile="emacs --batch --eval \'(byte-recompile-directory \"~/.emacs.d\" 0)'"

# gcc
gcc-init()
{
   if [ -z "$1" ]; then
      echo "Missing gcc version; exiting..."
      return
   fi
   echo "Using gcc $1"
   cp -f ~/config/env/gcc/"$1"/.env ./.gcc-env
}
gcc-setup() { source .gcc-env; }

# SNAP
snap-init()
{
   if [ -z "$1" ]; then
      echo "Missing snap version; exiting..."
      return
   fi
   echo "Using snap version $1"
   cp -f ~/config/env/snap/"$1"/.env ./.snap-env
   cd snap/.git/hooks &&
      ln -sf -T ../../buildtools/pre-commit pre-commit &&
      cd ../../.. &&
      mkdir -p sim mock debug release
}
snap-setup()
{
   export SNAP_ROOT=`pwd` && source .snap-env;
}

export ONE_TICK_CONFIG=/opt/1tick/client_data/config/one_tick_config.txt

#export LD_LIBRARY_PATH=/usr/local/snap/lib:$LD_LIBRARY_PATH
# use gcc451
