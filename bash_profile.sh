# environment variables
## PATH start
QT_PATH=~/opt/Qt5.5.1/5.5/clang_64/bin
export PATH=~/bin:~/.cabal/bin:$QT_PATH:~/.rbenv/shims:$PATH
## PATH end

alias be='bundle exec'
alias lsl="ls | grep '^[a-z]'"
alias lsu="ls | grep '^[A-Z]'"


if [ -d /usr/local/cuda ]; then
    export LD_LIBRARY_PATH=/usr/local/cuda/lib64
fi

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


# docker-machine
export DOCKER_API_VERSION=1.22

# Docker cleanup shortcuts
function docker-clean-exited() {
    if [[ $(docker ps -a -q -f status=exited | wc -l) -eq 0 ]]; then
        echo 'Exited docker containers are already clean.  :)'
    else
        docker rm -v $(docker ps -a -q -f status=exited)
    fi
}
function docker-clean-images() {
    if [[ $(docker images -f "dangling=true" | wc -l) -eq 0 ]]; then
        echo 'Dangling docker images are already clean.  :)'
    else
        docker rmi $(docker images -f "dangling=true" -q)
    fi
}

alias dc="$(which docker-compose)"

function dcrun() {
    docker_cmd="$(which docker-compose)"
    container_name="$1"
    shift
    ${docker_cmd} run ${container_name} bash -c -l "$*"
}

function dbe() {
    dcrun web "bundle exec $*"
}

alias docker-clean='docker-clean-exited; docker-clean-images'
alias bp="bundle package --all --no-install"
alias dcu="docker-compose up"
alias dcd="docker-compose down"
