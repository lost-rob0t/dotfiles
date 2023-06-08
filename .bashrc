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
			. "$rc"
		fi
	done
fi

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

function install_doom_emacs () {
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

alias debug-emacs="emacs --debug-init"

alias emacs="emacs-clean"

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

eval "$(direnv hook bash)"

eval "$(starship init bash)"
