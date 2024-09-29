{ pkgs }:

pkgs.writeShellScriptBin "dmenu-exit" ''
#!/bin/sh

# Created By: Javier Pacheco - jpacheco@cock.li
# Created On: 23/10/2023
# Project: dmenu-exit script

# Options
shutdown="Shutdown"
reboot="Reboot"
hrestart="Restart Hyprland"
hibernate="Sleep"
lock="Lock"

chosen=$(printf "$reboot\n$shutdown\n$hrestart\n$hibernate\n$lock" | ${pkgs.tofi}/bin/tofi --prompt "Hyprland - Session: ")
case $chosen in

    $shutdown)
      notify-send  "El sistema se apagara en...." "$( echo "15 segundos" )" &
      sleep 5
      notify-send  "El sistema se apagara en...." "$( echo "10 segundos" )" &
      sleep 5 
      notify-send  "El sistema se apagara en...." "$( echo "5 segundos" )" &
      sleep 4
      systemctl shutdown -P now 
        ;;
    $reboot)
      systemctl reboot
        ;;
    $hrestart)
      pkill Hyprland
        ;;
    $lock)
      ${pkgs.hyprlock}/bin/hyprlock
      ;;
    *) notify-send "Ninguna opcion fue seleccionada..."
esac
''
