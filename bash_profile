# environment variables
export EDITOR=vim
export CLICOLOR=yes
export HISTSIZE=100000000 # 1e8 (10 million)
export PATH=/Users/tlehman/bin:$PATH
export VAGRANT_DEFAULT_PROVIDER=vmware_fusion

# aliases
alias lsl="ls | grep '^[a-z]'"
alias lsu="ls | grep '^[A-Z]'"

# colors
yellow="\[\033[1;33m\]"
green="\[\033[1;32m\]"
blue="\[\033[34m\]"
closecolor="\[\033[0m\]"

# functions for command prompt
function parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

function pwd_short() {
  pwd | sed 's/\/Users\/tlehman/\~/'
}

# command prompt
git_branch_if_applicable=""
export PS1="$(whoami)@$(hostname -s):$yellow\$(pwd_short)$closecolor$green\$(parse_git_branch)$closecolor $ "
