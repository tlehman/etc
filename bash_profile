# environment variables
export EDITOR=vim
export CLICOLOR=yes
export HISTSIZE=100000000 # 1e8 (10 million)
export PATH=~/bin:~/.rbenv/shims:$PATH
export GOPATH=~/go
export GPG_TTY=`tty`


# OS specific stuff
if [ $(uname) = "Linux" ]; then
    # turn CAPS LOCK into Control on Linux
    setxkbmap -layout us -option ctrl:nocaps
elif [ $(uname) = "Darwin" ]; then
    source /usr/local/Cellar/rbenv/0.4.0/completions/rbenv.bash
    source /usr/local/etc/bash_completion.d/git-completion.bash
fi

# vm tools
export VAGRANT_DEFAULT_PROVIDER=vmware_fusion

# aliases
alias lsl="ls | grep '^[a-z]'"
alias ls3="ls | grep '^...$'"
alias lsu="ls | grep '^[A-Z]'"

# colors
red="\[\033[1;31m\]"
yellow="\[\033[1;33m\]"
green="\[\033[1;32m\]"
blue="\[\033[34m\]"
closecolor="\[\033[0m\]"

# functions for command prompt
function parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

function pwd_short() {
    if [ $(uname) = "Linux" ]; then
        pwd | sed "s/\/home\/$(whoami)/\~/"
    elif [ $(uname) = "Darwin" ]; then
        pwd | sed "s/\/Users\/$(whoami)/\~/"
    fi
}

function git_repo_dirty() {
  if [[ $(git s 2>/dev/null | egrep '(modified|new file):' | wc -l) -ge 1 ]]; then 
  	echo "*"
  elif [[ $(git s 2>/dev/null | grep 'working directory clean' | wc -l) -le 1 ]]; then
    echo ""
  fi
}

# command prompt
git_branch_if_applicable=""
export PS1="$(whoami)@$(hostname -s):$yellow\$(pwd_short)$closecolor$green\$(parse_git_branch)$closecolor$red\$(git_repo_dirty)$closecolor $ "
