#!/usr/bin/env bash

# Use sudo or ELEVATE_CMD
get_elevate_cmd() {
    if command -v doas >/dev/null 2>&1; then
        echo "doas"
    elif [ -x "/usr/bin/sudo" ]; then  # Ensure sudo is installed but avoid triggering dependency checks
        echo "sudo"
    else
        echo "No doas or sudo found. Please install one." >&2
        exit 1
    fi
}

# Get the appropriate elevation command
ELEVATE_CMD=$(get_elevate_cmd)

# Ensure fzf is installed
if ! command -v fzf &>/dev/null; then
    echo "fzf is not installed. Please install it first."
    exit 1
fi

# Get a list of services and their statuses
list_services() {
    \ls /var/service/
}

# Select a service using fzf
# selected_service=$(list_services | fzf --prompt="Select a service: ")
selected_service=$(list_services | tofi --prompt-text "Select a service: ")

# Exit if no service was selected
if [[ -z "$selected_service" ]]; then
    echo "No service selected. Exiting..."
    exit 1
fi

# Action menu
# action=$(printf "Start\nStop\nRestart\nStatus" | fzf --prompt="Action for $selected_service: " --height=10 --reverse)
action=$(printf "Start\nStop\nRestart\nStatus" | tofi --prompt-text "Action for $selected_service: ")

# Perform the selected action
case $action in
    "Start")
        $ELEVATE_CMD sv start "$selected_service" && notify-send "Service Status" "$selected_service started."
        ;;
    "Stop")
        $ELEVATE_CMD sv stop "$selected_service" && notify-send "Service Status" "$selected_service stopped."
        ;;
    "Restart")
        $ELEVATE_CMD sv restart "$selected_service" && notify-send "Service Status" "$selected_service restarted."
        ;;
    "Status")
        $ELEVATE_CMD sv status "$selected_service"
        ;;
    *)
        echo "Invalid action selected."
        ;;
esac

