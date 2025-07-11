#!/usr/bin/env bash

# Check if fuzzel is installed
command -v fuzzel >/dev/null 2>&1 || {
  echo "fuzzel is not installed. Please install it."; exit 1;
}

# Get list of all systemd services
SERVICES=$(systemctl list-units --type=service  --no-pager --no-legend | awk '{print $1}')

# Let user pick a service with fuzzel
SELECTED_SERVICE=$(printf "%s\n" $SERVICES | fuzzel --dmenu --prompt "Select service: ")

# Exit if no service selected
[ -z "$SELECTED_SERVICE" ] && notify-send "Service Manager" "No service selected." && exit 0

# Let user choose an action
ACTION=$(printf "start\nstop\nrestart" | fuzzel --dmenu --prompt "Action for $SELECTED_SERVICE: ")

# Exit if no action selected
[ -z "$ACTION" ] && notify-send "Service Manager" "No action selected." && exit 0

# Confirm action
CONFIRM=$(printf "Yes\nNo" | fuzzel --dmenu --prompt "Are you sure you want to $ACTION $SELECTED_SERVICE? ")

if [[ "$CONFIRM" == "Yes" ]]; then
    if sudo systemctl "$ACTION" "$SELECTED_SERVICE"; then
        notify-send "Service Manager" "'$ACTION' executed on '$SELECTED_SERVICE'"
    else
        notify-send "Service Manager" "Failed to '$ACTION' '$SELECTED_SERVICE'"
    fi
else
    notify-send "Service Manager" "Action cancelled."
    exit 0
fi
