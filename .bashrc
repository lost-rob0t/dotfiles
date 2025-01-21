if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

if [ ! -d ~/.bashrc.d ]; then
	for rc in ~/.bashrc.d/*; do
		if [ -f "$rc" ]; then
			source "$rc"
		fi
	done
fi

export TERM="xterm-256color"                      # getting proper colors

shopt -s histappend

shopt -s cmdhist # save multi-line commands in history as single line

export HISTFILESIZE=
export HISTSIZE=
HISTCONTROL="ignoreboth"

HISTTIMEFORMAT="[%Y-%m-%d %H:%M:%S] "

export HISTIGNORE=fg:bg:ls:cd

__fzfcmd() {
  [[ -n "${TMUX_PANE-}" ]] && { [[ "${FZF_TMUX:-0}" != 0 ]] || [[ -n "${FZF_TMUX_OPTS-}" ]]; } &&
    echo "fzf-tmux ${FZF_TMUX_OPTS:--d${FZF_TMUX_HEIGHT:-40%}} -- " || echo "fzf"
}
if command -v perl > /dev/null; then
  __fzf_history__() {
    local output opts script
    opts="--height ${FZF_TMUX_HEIGHT:-40%} --bind=ctrl-z:ignore ${FZF_DEFAULT_OPTS-} -n2..,.. --scheme=history --bind=ctrl-r:toggle-sort ${FZF_CTRL_R_OPTS-} +m --read0"
    script='BEGIN { getc; $/ = "\n\t"; $HISTCOUNT = $ENV{last_hist} + 1 } s/^[ *]//; print $HISTCOUNT - $. . "\t$_" if !$seen{$_}++'
    output=$(
      set +o pipefail
      builtin fc -lnr -2147483648 |
        last_hist=$(HISTTIMEFORMAT='' builtin history 1) command perl -n -l0 -e "$script" |
        FZF_DEFAULT_OPTS="$opts" $(__fzfcmd) --query "$READLINE_LINE"
    ) || return
    READLINE_LINE=${output#*$'\t'}
    if [[ -z "$READLINE_POINT" ]]; then
      echo "$READLINE_LINE"
    else
      READLINE_POINT=0x7fffffff
    fi
  }
else # awk - fallback for POSIX systems
  __fzf_history__() {
    local output opts script n x y z d
    if [[ -z $__fzf_awk ]]; then
      __fzf_awk=awk
      # choose the faster mawk if: it's installed && build date >= 20230322 && version >= 1.3.4
      IFS=' .' read n x y z d <<< $(command mawk -W version 2> /dev/null)
      [[ $n == mawk ]] && (( d >= 20230302 && (x *1000 +y) *1000 +z >= 1003004 )) && __fzf_awk=mawk
    fi
    opts="--height ${FZF_TMUX_HEIGHT:-40%} --bind=ctrl-z:ignore ${FZF_DEFAULT_OPTS-} -n2..,.. --scheme=history --bind=ctrl-r:toggle-sort ${FZF_CTRL_R_OPTS-} +m --read0"
    [[ $(HISTTIMEFORMAT='' builtin history 1) =~ [[:digit:]]+ ]]    # how many history entries
    script='function P(b) { ++n; sub(/^[ *]/, "", b); if (!seen[b]++) { printf "%d\t%s%c", '$((BASH_REMATCH + 1))' - n, b, 0 } }
    NR==1 { b = substr($0, 2); next }
    /^\t/ { P(b); b = substr($0, 2); next }
    { b = b RS $0 }
    END { if (NR) P(b) }'
    output=$(
      set +o pipefail
      builtin fc -lnr -2147483648 2> /dev/null |   # ( $'\t '<lines>$'\n' )* ; <lines> ::= [^\n]* ( $'\n'<lines> )*
        command $__fzf_awk "$script"           |   # ( <counter>$'\t'<lines>$'\000' )*
        FZF_DEFAULT_OPTS="$opts" $(__fzfcmd) --query "$READLINE_LINE"
    ) || return
    READLINE_LINE=${output#*$'\t'}
    if [[ -z "$READLINE_POINT" ]]; then
      echo "$READLINE_LINE"
    else
      READLINE_POINT=0x7fffffff
    fi
  }
fi

bind -m emacs-standard -x '"\C-r": __fzf_history__'

source ~/.dotfiles/.bash.d/fzf-bash-cmpl.sh
bind -x '"\t": fzf_bash_completion'

function init_platform () {
# create the .platform file i use
if [ ! -f "$HOME/.platform" ]; then
    echo "no .platform file please enter platform name"
    read platform
    echo $platform > $HOME/.platform
fi
}
init_platform

alias get-ip="curl -s -q ifconfig.me"
get-ip > "$HOME/.local/share/ip"

ex ()
{
  if [ -f "$1" ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1   ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *.deb)       ar x $1      ;;
      *.tar.xz)    tar xf $1    ;;
      *.tar.zst)   unzstd $1    ;;
      *.tar.zstd)   unzstd $1    ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

export ALTERNATE_EDITOR=""                        # setting for emacsclient
export EDITOR="emacsclient -t -a ''"              # $EDITOR use Emacs in terminal
export VISUAL="emacsclient -c -a emacs"           # $VISUAL use Emacs in GUI mode

function install-doom () {
 if [ -d ~/.emacs.d ]; then
 	echo "Are you sure you want to delete ~/.emacs.d/ directory and install doom emacs? (y/n)"
    read -p "$* [y/n]: " yn
    case $yn in
        [Yy]*) rm -rvf ~/.emacs.d &&  git clone --depth 1 https://github.com/doomemacs/doomemacs.git ~/.emacs.d/ &&  ~/.emacs.d/bin/doom install;;
        [Nn]*) echo "Aborted";;
    esac
 fi
}

function emacs-clean () {
if [ $# -eq 0 ]; then
    emacsclient -c -n
    exit
fi

emacsclient -e "(frames-on-display-list \"$DISPLAY\")" &>/dev/null

if [ $? -eq 0 ]; then
    emacsclient -n "$*"
else
    emacsclient -c -n "$*"
fi
}

function evolve () {
read -p "Do you want to rebuild the config? (yes/no) " yn

case $yn in
    yes ) echo ok, we will proceed;;
    no ) echo exiting...;
         exit;;
    * ) echo invalid response;
        exit 1;;
esac
sudo cp -rv $HOME/nixos/* /etc/nixos/
sudo sed  -i "s|<config>|$1/$1.nix|" /etc/nixos/configuration.nix
if [ "$2" = "" ];then
    sudo nixos-rebuild switch
else
nixos-rebuild "$@"
fi
echo done
}

if [ ! -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
    source ~/.nix-profile/etc/profile.d/nix.sh
    export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}
fi

function nim-init () {
 # Init a nim project and start a git repo
 nimble init $1
 git init "$PWD/$1"
}

function cmdtop () {
    history | awk '{CMD[$2]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}' | grep -v "./" | column -c3 -s " " -t | sort -nr | nl |  head -n10
}

export PATH=$PATH:$HOME/.nimble/bin

export PATH=$PATH:$HOME/.cargo/bin

export PATH=$PATH:$HOME/.bin/

export PATH=$PATH:$HOME/.node/bin/

export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

shopt -s expand_aliases # expand aliases

alias debug-emacs="emacs --debug-init"

alias em="emacs -nw"
alias emacs="emacsclient -c -a 'emacs'"

alias nix-xdg-link="ln -s ~/.nix-profile/share/applications/ ~/.local/share/applications/nix"

alias nim-doc="nim doc --project --index:on --outdir=docs"

# aliases for grc(1)

# this will execute only if there is a line with
# GRC_ALIASES=true
# in /etc/default/grc or you export GRC_ALIASES=true prior to sourcing this

[ -f /etc/default/grc ] && . /etc/default/grc


GRC="$(which grc)"
if tty -s && [ -n "$TERM" ] && [ "$TERM" != dumb ] && [ -n "$GRC" ]; then
    alias colourify="$GRC -es"
    alias blkid='colourify blkid'
    alias configure='colourify ./configure'
    alias df='colourify df'
    alias diff='colourify diff'
    alias docker='colourify docker'
    alias docker-compose='colourify docker-compose'
    alias docker-machine='colourify docker-machine'
    alias du='colourify du'
#    alias env='colourify env'
    alias free='colourify free'
    alias fdisk='colourify fdisk'
    alias findmnt='colourify findmnt'
    alias make='colourify make'
    alias gcc='colourify gcc'
    alias g++='colourify g++'
    alias id='colourify id'
    alias ip='colourify ip'
    alias iptables='colourify iptables'
    alias as='colourify as'
    alias gas='colourify gas'
    alias journalctl='colourify journalctl'
    alias kubectl='colourify kubectl'
    alias ld='colourify ld'
    #alias ls='colourify ls'
    alias lsof='colourify lsof'
    alias lsblk='colourify lsblk'
    alias lspci='colourify lspci'
    alias netstat='colourify netstat'
    alias ping='colourify ping'
    alias ss='colourify ss'
    alias traceroute='colourify traceroute'
    alias traceroute6='colourify traceroute6'
    alias head='colourify head'
    alias tail='colourify tail'
    alias dig='colourify dig'
    alias mount='colourify mount'
    alias ps='colourify ps'
    alias mtr='colourify mtr'
    alias semanage='colourify semanage'
    alias getsebool='colourify getsebool'
    alias ifconfig='colourify ifconfig'
    alias sockstat='colourify sockstat'
fi

alias wttr="curl wttr.in"

alias couchdb="mkdir -p $PWD/.database && sudo chown 1001:1001 $PWD/.database && sudo docker run -d  -e COUCHDB_USER=admin -e COUCHDB_PASSWORD=password  -v $PWD/.database:/opt/couchdb/data  -p 0.0.0.0:5984:5984 ibmcom/couchdb3" && echo $PWD/.database >> $HOME/.config/couchdb-databases

alias couchdb-gc="grep -e '\.database$' ~/.config/couchdb-databases | xargs -I {} sudo rm -rf {} && rm -f ~/.config/couchdb-databases && touch ~/.config/couchdb-databases"

function couchdb-rm-db() {
  selected_file=$(cat ~/.config/couchdb-databases | sort -u | fzf)
  if [ -n "$selected_file" ]; then
    sed -i "\~$selected_file~d" ~/.config/couchdb-databases
    sudo rm -rfv "$selected_file"
  fi
}

alert_cmd=$(which "dunstify" || which "notify-send")
alias alert='$alert_cmd --urgency=medium -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

alias paste="curl -F 'f:1=<-' ix.io"
alias ix.io="curl -F 'f:1=<-' ix.io"

alias hackmode="cd $HOME/Documents/hackmode"

alias starintel="cd ~/Documents/Projects/starintel"

alias reload-bash="source $HOME/.bashrc"

alias unix="date +%s"

alias npm-init-dir="mkdir -p ~/.node"
alias npm-install="npm install --prefix ~/.node -g"

alias ai-proxy="ssh -N -L 7860:127.0.0.1:7860 unseen@10.50.50.18"

alias sync-music="rsync --progress -av ~/Music/Music-Sorted/ proxmox:/mnt/Music && ssh proxmox python3 /mnt/sort.py /mnt/Music"

export HACKMODE_OP=$(cat ~/.local/share/hackmode/current-op | head -n 1)
export HACKMODE_PATH=$(cat ~/.local/share/hackmode/op-path | head -n 1)
export HACKMODE_BASE_DIR="/home/$USER/Documents/hackmode/"
function shm() {
  selected_dir=$(find "$HACKMODE_BASE_DIR" -maxdepth 1 -type d | fzf)
  if [ -n "$selected_dir" ]; then
    export HACKMODE_OP=$(basename "$selected_dir")
    export HACKMODE_PATH="$selected_dir"
    echo "$HACKMODE_OP" > ~/.local/share/hackmode/current-op
    echo "$HACKMODE_PATH" > ~/.local/share/hackmode/op-path
    cd "$selected_dir"
  fi
}

function hackmode-setting() {
  if [ -z "$HACKMODE_OP" ]; then
    echo "HACKMODE_OP is not set. Please select a hackmode directory using 'shm' first."
    return 1
  fi

  settings_dir="$HACKMODE_PATH/.config/$HACKMODE_OP"

  # Create settings directory if it doesn't exist
  if [ ! -d "$settings_dir" ]; then
    mkdir -p "$settings_dir"
    read -p "Enter the name of the setting: " setting_name
  else
    setting_name=$(basename =$(find "$settings_dir" -type f | fzf ))
  fi

  # Use the specified editor or fallback to a default editor (e.g., nano)
  editor=${VISUAL:-$EDITOR}
  editor=${editor:-nano}

  # Prompt user for setting name
  if [ -n "$setting_name" ]; then
    setting_file="$settings_dir/$setting_name"
    $editor "$setting_file"
  else
    echo "Setting name cannot be empty."
  fi
}


function list-hackmode-settings () {

  if [ -z "$HACKMODE_OP" ]; then
    echo "HACKMODE_OP is not set. Please select a hackmode directory using 'shm' first."
    return 1
  fi

  settings_dir="$HACKMODE_PATH/.config/"

  if [ ! -d "$settings_dir" ]; then
    echo "Settings directory not found: $settings_dir"
    return 1
  fi

  for setting_file in "$settings_dir"/*; do
    setting_name=$(basename "$setting_file")

    if [ -f "$setting_file" ]; then
      while IFS= read -r line; do
        echo "$setting_name: $line"
      done < "$setting_file"
    fi
  done
}

alias cdhm="cd $HACKMODE_PATH"

export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export ANSIBLE_HOME="${XDG_CONFIG_HOME}/ansible"
export ANSIBLE_CONFIG="${XDG_CONFIG_HOME}/ansible.cfg"
export ANSIBLE_GALAXY_CACHE_DIR="${XDG_CACHE_HOME}/ansible/galaxy_cache"
export RECOLL_CONFDIR="$XDG_CONFIG_HOME/recoll"
export DOCKER_CONFIG="$XDG_CONFIG_HOME/docker"
export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME"/java

function fancy-shell () {
    eval "$(direnv hook bash)"
    eval "$(starship init bash)"
}

case $TERM in
    xterm-256color)
        fancy-shell;;
    xterm)
        fancy-shell;;
    *)
        PS1="$";;
esac
