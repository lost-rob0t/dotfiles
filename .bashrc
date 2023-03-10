# .bashrc

NIX_CONFIG="neptune/neptune.nix"

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
if [ ! -d ~/.bashrc.d ]; then
	for rc in ~/.bashrc.d/*; do
		if [ -f "$rc" ]; then
			. "$rc"
		fi
	done
fi

unset rc

function install_doom_emacs () {
 if [ -d ~/.emacs.d ]; then 
 	echo "Are you sure you want to delete ~/.emacs.d/ directory and install doom emacs? (y/n)"
    while true; do
        read -p "$* [y/n]: " yn
        case $yn in
            [Yy]*) rm -rvf ~/.emacs.d &&  git clone --depth 1 git@lost-git.local:MirrorBot/doom-emacs.git ~/.emacs.d/ &&  ~/.emacs.d/bin/doom install;;
            [Nn]*) echo "Aborted" ; exit ;;
        esac
    done
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

function evolve-home () {
read -p "Do you want to rebuild your home config? (yes/no) " yn

case $yn in
    yes ) echo ok, we will proceed;;
    no ) echo exiting...;
         exit;;
    * ) echo invalid response;
        exit 1;;
esac
sudo sed  -i "s|<config>|$1/$1.nix|" /etc/nixos/configuration.nix
if [ "$2" = "" ];then
    sudo nixos-rebuild switch
else
nixos-rebuild "$@"
fi
echo done
}

function make_password () {
## TODO dont echo password to terminal lol
echo "enter password: "
read password
pass=$(mkpasswd -m sha-512 $password)
echo "$pass"
}
function init_platform () {
# create the .platform file i use
if [ ! -f "$HOME/.platform" ]; then
    echo "no .platform file please enter platform name"
    echo ": "
    read platform
    echo platform > $HOME/.platform
fi
}
# Custome aliases for commands
alias install-doom="install_doom_emacs"
alias docker-compose="podman-compose"
alias wttr="curl wttr.in/DCA"
alias debug-emacs="emacs --debug-init"
alias nix-xdg-link="ln -s ~/.nix-profile/share/applications/ ~/.local/share/applications/nix"
alias nim-doc="nim doc --project --index:on --outdir=docs"
## For non nix systems
#alias nix="~/.nix-profile/etc/profile.d/nix.sh"
### Shell init section
source ~/.nix-profile/etc/profile.d/nix.sh
export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}
# Setup nim path
export PATH=$PATH:$HOME/.nimble/bin
# rust path ewww
export PATH=$PATH:$HOME/.cargo/bin
#export LD_LIBRARY_PATH=/usr/local/lib
init_platform
eval "$(direnv hook bash)"
eval "$(starship init bash)"

#(cat ~/.cache/wal/sequences &)
