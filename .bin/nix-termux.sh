#!/bin/sh -ue

# This script installs Nix package manager inside a Termux installation.
# Copyright (c) 2019 Alexander Sosedkin <monk@unboiled.info>

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


# Based off the official Nix install script (https://nixos.org/nix/install),
# presumably written by Eelco Dolstra.


# This script installs Nix package manager inside a Termux installation.
# It does not require root, user namespaces or disabling SELinux,
# but it relies on proot and numerous hacks instead.
# Only tested with aarch64, may also accidentally work on x86.

# Usage:
# * Install and run Termux
# * Execute this script inside Termux

# Careful, this is carelessly written alpha-quality stuff.


PLAYGROUND="$HOME/.nix"
DIVE_SCRIPT="$PLAYGROUND/nix-powered"
TERMUX_BIN="/data/data/com.termux/files/usr/bin"
# update the version number
NIX_VERSION="nix-2.9.2"
BASE_URL="https://releases.nixos.org/nix/$NIX_VERSION/"

oops() {
    echo "$0:" "$@" >&2
    exit 1
}



if [ ! -x "$DIVE_SCRIPT" ]; then
	echo "removing the previous installation..."
	chmod -R 700 "$PLAYGROUND" || true
	rm -rf "$PLAYGROUND" || oops "failed to remove $PLAYGROUND"
	mkdir -p "$PLAYGROUND" || oops "failed to create $PLAYGROUND"

	echo "installing tools..."
	pkg install -y proot bzip2 tar wget || oops "failed to install required tools"

	tmpDir="$PLAYGROUND/tmp"
	mkdir -p "$tmpDir" || oops "false to create $tmpDir"

	echo "preparing mock /nix and /etc..."
	mkdir "$PLAYGROUND/nix" || oops "failed to create $PLAYGROUND/nix"
	mkdir "$PLAYGROUND/etc" || oops "failed to create $PLAYGROUND/etc"
	echo 'nameserver 1.1.1.1' > "$PLAYGROUND/etc/resolv.conf" || oops "failed to create $PLAYGROUND/etc/resolv.conf"
	mkdir "$PLAYGROUND/etc/nix/" || oops "failed to create $PLAYGROUND/etc/nix"
	# https://github.com/NixOS/nix/issues/2632#issuecomment-457610729
	echo 'sandbox = false' > "$PLAYGROUND/etc/nix/nix.conf" || oops "failed to create $PLAYGROUND/etc/nix/nix.conf"

	case "$(uname -s).$(uname -m)" in
		Linux.x86_64) system=x86_64-linux;;
		Linux.i?86) system=i686-linux;;
		Linux.aarch64) system=aarch64-linux;;
		Darwin.x86_64) system=x86_64-darwin;;
		*) oops "sorry, there is no binary distribution of Nix for your platform";;
	esac

	url="$BASE_URL/$NIX_VERSION-$system.tar.xz"
    hash=$($TERMUX_BIN/curl "$url.sha256" -s)
    echo $url
	tarball="$tmpDir/$(basename "$tmpDir/$NIX_VERSION-$system.tar.xz")"
	#tarball="$HOME/nix-2.2.1-$system.tar.bz2"

	echo "downloading $NIX_VERSION binary tarball for $system from '$url' to '$tmpDir'..."
	$TERMUX_BIN/wget "$url" -O "$tarball" || oops "failed to download '$url'"

        hash2="$(sha256sum -b "$tarball" | cut -c1-64)"

	if [ "$hash" != "$hash2" ]; then
		oops "SHA-256 hash mismatch in '$url'; expected $hash, got $hash2"
	fi

	echo "unpacking $tarball..."
	unpack=$tmpDir/unpack
	mkdir -p "$unpack"
	cleanup() {
		rm -rf "$unpack"
	}
	trap cleanup EXIT INT QUIT TERM
	< "$tarball" bzcat | $TERMUX_BIN/tar -xf - -C "$unpack" || oops "failed to unpack $tarball"

	script=$(echo "$unpack"/*/install)

	[ -e "$script" ] || oops "installation script is missing from the binary tarball!"

	INTERMEDIATE_SCRIPT="$tmpDir/intermediate_install_script"
	{
########
	cat > "$INTERMEDIATE_SCRIPT" << --------EOF
	export USER=\$(whoami)
	export NIX_INSTALLER_NO_MODIFY_PROFILE=true
	unset LD_LIBRARY_PATH
	unset LD_PRELOAD
	$script "\$@"
--------EOF
	} || oops "failed to create $INTERMEDIATE_SCRIPT"

	chmod +x "$INTERMEDIATE_SCRIPT" || oops "failed to chmod +x $INTERMEDIATE_SCRIPT"

	echo "running Nix install script in proot..."
	proot \
		-b "$PLAYGROUND/nix:/nix" \
		-b "$PLAYGROUND/etc:/etc" \
		--link2symlink \
		"$INTERMEDIATE_SCRIPT" || oops "failed to run Nix install script in proot"

	echo "creating $DIVE_SCRIPT and finalizind the installation..."
	{
########
	cat > "$DIVE_SCRIPT" << --------EOF
	#!/bin/sh -e
	export PS1="nix-powered> "
	export USER=$(whoami)
	unset LD_LIBRARY_PATH
	unset LD_PRELOAD
	echo Termux PATH \$PATH
	TERMUX_PATH="\$PATH"
	. \$HOME/.nix-profile/etc/profile.d/nix.sh
	export PATH=\${PATH%":\$TERMUX_PATH"}
	"\$@"
--------EOF
	} || oops "failed to create $DIVE_SCRIPT"
	chmod +x "$DIVE_SCRIPT" || oops "failed to chmod +x $DIVE_SCRIPT"

	echo "installation is completed."

	echo "to reinstall, rm -r \"$PLAYGROUND\" and re-run the script"
fi


proot \
	-b "$PLAYGROUND/nix:/nix" \
	-b "$PLAYGROUND/etc:/etc" \
	-b "/etc:/android-etc" \
	--link2symlink \
	"$DIVE_SCRIPT" "$@"
