#!/usr/bin/env bash

# Check if tofi is installed
if ! command -v tofi &> /dev/null
then
    echo "tofi could not be found"
    exit
fi

# Check if pass is installed
if ! command -v pass &> /dev/null
then
    echo "pass could not be found"
    exit
fi

PASS=""
RET=""

# Check if PASSWORD_STORE_DIR is set
if [ -z "$PASSWORD_STORE_DIR" ]
then
    PASSWORD_STORE_DIR="$HOME/.password-store"
fi

# Get all the passwords
PASS=$(find "$PASSWORD_STORE_DIR" -name '*.gpg' | sed "s|$PASSWORD_STORE_DIR\/||g" | sed 's/.gpg//g')

# Get the password to copy
PASS=$(echo "$PASS" | tofi --prompt-text "Select a password to copy: " 2>/dev/null)

# Exit if the user cancels
if [ -z "$PASS" ]
then
    exit
fi

# Copy the password to the clipboard
RET=$(pass show --clip "$PASS")

# Notify if notif-send is installed
if command -v notify-send &> /dev/null
then
    notify-send -t 45000 "tofi-pass" "$RET"
fi
