#!/bin/sh

# Created By: Javier Pacheco - javier@jpacheco.xyz
# Created On: 21/06/24
# Project: popup waybar

# Start Waybar
waybar &

# Get the PID of the Waybar process
WAYBAR_PID=$!

# Wait for 15 seconds
sleep 5

# Kill the Waybar process to hide it
kill $WAYBAR_PID
