#!/bin/sh

# Created By: Javier Pacheco - javier@jpacheco.xyz
# Created On: 13/04/24
# Project: Waybar module that displays outaded venom ports

threshhold_green=0
threshhold_yellow=1
threshhold_red=5

if ! updates_venom=$(doas scratch outdate | grep -v "masked" 2> /dev/null | wc -l ); then
    updates_venom=0
fi

updates=$updates_venom
list_pkgs=$(scratch outdate | grep -v "masked" | tr '\n' '\r')

css_class="green"

if [ "$updates" -gt $threshhold_green ]; then
    css_class="yellow"
fi

if [ "$updates" -gt $threshhold_red ]; then
    css_class="red"
fi

if [ "$updates" -gt $threshhold_green ]; then
    printf '{"text": "%s  ", "alt": "%s", "tooltip": "%s", "class": "%s"}' "$updates" "$updates" "$list_pkgs" "$css_class"
else
    printf '{"text": " ", "alt": "0", "tooltip": "Venom linux updated", "class": "green"}'
fi
echo $list_pkgs
