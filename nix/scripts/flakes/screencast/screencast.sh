#!/bin/sh
# Created By: Javier Pacheco - javier@jpacheco.xyz
# Created On: 29/03/24
# Project: Screen recorder in Wayland
# Dependencies: wf-recorder, wl-copy, and a launcher like dmenu, fuzzel, etc.

SOUND_CARD=$(pactl list sources | awk '/Name/ && /.monitor/ {print $2}')

screencast() {
   wf-recorder --audio=$SOUND_CARD -f /tmp/screencast.mp4
}

area() {
   wf-recorder --audio=$SOUND_CARD -g "$(slurp)" -f /tmp/screencast.mp4
}

check_connection() {
    ping -c 1 google.com 1> /dev/null 2>&1
}

share() {
    hyprctl notify 5 5000 "rgb(458588)" "fontsize:15 Uploading video to 0x0.st"
    curl -F "file=@/tmp/screencast.mp4" https://0x0.st | wl-copy && hyprctl notify 5 5000 "rgb(458588)" "fontsize:15 Video uploaded to 0x0.st"
}

kill_proc(){
    pkill --signal SIGINT wf-recorder
    if [ $? -eq 0 ]; then
        hyprctl notify 5 5000 "rgb(458588)" "fontsize:15 Video saved to /tmp/screencast.mp4"
        exit 0
    fi
}

remove_vid() {
    [ -f /tmp/screencast.mp4 ] && rm /tmp/screencast.mp4
}

# Function to ask for upload confirmation
ask_upload() {
    UPLOAD=$(printf "yes\nno" | tofi --prompt 'Upload video to 0x0.st? ')
    case $UPLOAD in
        'yes')
            check_connection && share || hyprctl notify 3 5000 "rgb(ff5566)" "fontsize:15 Upload failed. Video saved in /tmp/screencast.mp4";;
        'no'|*) 
            hyprctl notify 5 5000 "rgb(458588)" "fontsize:15 Video saved to /tmp/screencast.mp4";;
    esac
}

kill_proc

OPT=$(printf "screencast\narea\nshare\nquit" | tofi --prompt 'Select an option: ')
case $OPT in
    'screencast')
        sleep 1
        remove_vid
        sleep 1
        screencast
        ask_upload;;
    'area')
        sleep 1
        remove_vid
        sleep 1
        area
        ask_upload;;
    'share') 
        check_connection && share || hyprctl notify 3 5000 "rgb(ff5566)" "fontsize:15 Video saved to /tmp/screencast.mp4";;
    *|quit) exit 0;;
esac
