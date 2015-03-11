# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

PATH=$PATH:$HOME/bin

export PATH

# Set proper permissions on ~/.ssh directory

dotssh="${HOME}/.ssh"
sshconf="${dotssh}/config"

if [ -d "${dotssh}" ]; then
   chmod 700 ${dotssh}
   if [ -f "${sshconf}" ]; then
       chmod 600 ${sshconf}
   fi
fi

unset dotssh
unset sshconf
