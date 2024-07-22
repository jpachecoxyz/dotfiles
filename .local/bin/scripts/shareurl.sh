#!/bin/sh

# Created By: Javier Pacheco - jpacheco@cock.li
# Created On: 27/03/24
# Project: Screen share script to store a quick img to 0x0.st

MENU="tofi"

OUTPUT_FILE="/tmp/capture.png"

OPT=$(printf "desktop\nwindow\nselection\nshare\nquit" | tofi --prompt "Select a capture option: ")

# capture the desktop
desktop() {
    grimblast save screen $OUTPUT_FILE && notify-send "Image stored in /tmp folder"
}

# capture focused window
window() {
    grimblast save active $OUTPUT_FILE && notify-send "Image stored in /tmp folder"
}

# select region
selection() {
    grimblast save area $OUTPUT_FILE && notify-send "Image stored in /tmp folder"
}

# share desktop [ https://0x0.st ]

check_connection() {
    ping -c 1 google.com 1> /dev/null 2>&1
}

share() {
    curl -F "file=@/tmp/capture.png" https://0x0.st | wl-copy && notify-send "Image stored in 0x0.st"
}

case $OPT in
    desktop)
	sleep 1
	desktop
	;;
    window)
	sleep 1
	window
        ;;
    selection)
	sleep 1
        selection
        ;;
    share) 
        check_connection && share || notify-send "Error" "check your internet connection" ;;
    *|quit)
	exit 0
	;;
esac
