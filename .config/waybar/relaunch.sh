#!/usr/bin/env sh

pkill waybar # Kill all instances of waybar
waybar -c ~/.config/waybar/jpbar/config.jsonc -s ~/.config/waybar/jpbar/style.css
