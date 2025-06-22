#!/bin/sh

# Created By: Javier Pacheco - javier@jpacheco.xyz
# Created On: 25/07/24
# Project: tombs scripts

TOMB_DIR="$HOME/.personal/"
KEY_FILE="$TOMB_DIR/personal.key"

ACTION=$(echo -e "💀 Raise the dead (open)\n🪦 Rest in peace (close)\n🔮 Tomb Slam All" | fzf --prompt="Choose your action: ")

if [ "$ACTION" = "💀 Raise the dead (open)" ]; then
    TOMB_FILE=$(find "$TOMB_DIR" -type f -name "*.tomb" | fzf --prompt="Select a tomb to open: ")
    if [ -n "$TOMB_FILE" ]; then
        echo "🔓 Opening the crypt..."
        tomb open "$TOMB_FILE" ~/public -k "$KEY_FILE"
    else
        echo "No tomb selected. Returning to the crypt."
    fi
elif [ "$ACTION" = "🪦 Rest in peace (close)" ]; then
    echo "🔒 Sealing the tomb..."
    tomb --sudo doas close
elif [ "$ACTION" = "🔮 Tomb Slam All" ]; then
    echo "🔒 Slamming all tombs shut..."
    tomb --sudo doas slam all
else
    echo "No action selected. The spirits remain restless."
fi
