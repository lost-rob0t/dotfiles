# .bashrc

NIX_CONFIG="desktop/desktop.nix"

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
sudo sed  -i "s|<config>|$NIX_CONFIG |" /etc/nixos/configuration.nix
if [ "$1" = "" ];then
    sudo nixos-rebuild switch
else
nixos-rebuild "$@"
fi
echo done
}
# Custome aliases for commands
alias install-doom="install_doom_emacs"
alias docker-compose="podman-compose"
alias wttr="curl wttr.in/CMH"
# Setup nim path
export PATH=/home/nsaspy/.nimble/bin:$PATH
#export LD_LIBRARY_PATH=/usr/local/lib
eval "$(direnv hook bash)"
