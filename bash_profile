# environment variables
## PATH start
QT_PATH=~/opt/Qt5.5.1/5.5/clang_64/bin
export PATH=~/bin:$QT_PATH:~/.rbenv/shims:$PATH
## PATH end

alias be='bundle exec'

export EDITOR=vim
export CLICOLOR=yes
export HISTSIZE=100000000 # 1e8 (10 million)
export GPG_TTY=`tty`
source ~/.api_env

# OS specific stuff
if [ "$(uname)" = "Linux" ]; then
    # turn CAPS LOCK into Control on Linux
    setxkbmap -layout us -option ctrl:nocaps
elif [ "$(uname)" = "Darwin" ]; then
    source /usr/local/Cellar/rbenv/1.0.0/completions/rbenv.bash
    source /usr/local/etc/bash_completion.d/git-completion.bash
    alias wolfram=/Applications/Mathematica.app/Contents/MacOS/MathKernel
fi

# colors
export red="\[\033[1;31m\]"
export yellow="\[\033[1;33m\]"
export green="\[\033[1;32m\]"
export blue="\[\033[34;1m\]"
export closecolor="\[\033[0m\]"

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
nameathost="$green$(whoami)@$(hostname -s)$closecolor"
function shell_git() {
    echo "$(parse_git_branch)$(git_repo_dirty)"
}
# \w means PWD: http://unix.stackexchange.com/a/100961/14306
export PS1="$nameathost:$blue\w$closecolor \$(shell_git)$ "

hitch() {
  command hitch "$@"
  if [[ -s "$HOME/.hitch_export_authors" ]] ; then source "$HOME/.hitch_export_authors" ; fi
}
alias unhitch='hitch -u'
