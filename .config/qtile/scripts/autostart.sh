#!/usr/bin/env bash

function run {
  if ! pgrep -x $(basename $1 | head -c 15) 1>/dev/null; then
    $@ &
  fi
}

#Set your native resolution IF it does not exist in xrandr
#More info in the script
#run $HOME/.config/qtile/scripts/set-screen-resolution-in-virtualbox.sh

#Find out your monitor name with xrandr or arandr (save and you get this line)
#xrandr --output VGA-1 --primary --mode 1360x768 --pos 0x0 --rotate normal
#xrandr --output DP2 --primary --mode 1920x1080 --rate 60.00 --output LVDS1 --off &
#xrandr --output LVDS1 --mode 1366x768 --output DP3 --mode 1920x1080 --right-of LVDS1
#xrandr --output HDMI2 --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output VIRTUAL1 --off
#autorandr horizontal

#change your keyboard if you need it
#setxkbmap -layout be

keybLayout=$(setxkbmap -v | awk -F "+" '/symbols/ {print $2}')

if [ $keybLayout = "be" ]; then
  cp $HOME/.config/qtile/config-azerty.py $HOME/.config/qtile/config.py
fi

#Some ways to set your wallpaper besides variety or nitrogen
#feh --bg-fill /usr/share/backgrounds/archlinux/arch-wallpaper.jpg &
# feh --bg-fill "/home/unseen/Pictures/wallpaper/10 - B6GTMQL.png" &
#wallpaper for other Arch based systems
#feh --bg-fill /usr/share/archlinux-tweak-tool/data/wallpaper/wallpaper.png &
#start the conky to learn the shortcuts
(conky -c $HOME/.config/qtile/scripts/system-overview) &

#start sxhkd to replace Qtile native key-bindings
run sxhkd -c ~/.config/qtile/sxhkd/sxhkdrc &

run "$HOME/.dotfiles/.config/qtile/scripts/graphics/$(hostname).sh"
#starting utility applications at boot time
run variety &
run nm-applet &
#run pamac-tray &
run xfce4-power-manager &
numlockx on &
run blueman-applet &
run picom --config $HOME/.config/picom/picom.conf &
run /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
run /usr/lib/xfce4/notifyd/xfce4-notifyd &
run spice-vdagent &
#starting user applications at boot time
run volumeicon &
run /home/unseen/.bin/graphics.sh &
run brave &
run bash /home/unseen/.dotfiles/.config/qtile/scripts/pinger.sh $HOME/.config/hosts &
run kdeconnect-indicator &
run aw-qt &
