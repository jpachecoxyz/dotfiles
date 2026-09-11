#!/bin/bash
set -euo pipefail

PLUGINS_DIR="$HOME/.config/omarchy/plugins"
BACKUP_DIR="$HOME/.dotfiles/omarchy/plugins"

declare -A ENABLED_PLUGINS=(
  [io.github.0x1ocean.omatrix]="https://github.com/0x1ocean/omarchy-omatrix.git"
  [io.github.tuthan.dropdown-terminal]="https://github.com/tuthan/omarchy-dropdown-terminal.git"
  [omarchy-overview]="https://github.com/AyushKr2003/omarchy-overview.git"
  [rosakodu.dock]="https://github.com/rosakodu/omarchy-dock.git"
)

declare -A DISABLED_PLUGINS=(
  [ericvrp.bar-autohide]="https://github.com/ericvrp/omarchy-bar-autohide.git"
  [io.github.huligabuliga.omasticky]="https://github.com/huligabuliga/Omasticky.git"
  [io.github.kristoferlund.webcam]="https://github.com/kristoferlund/omarchy-webcam.git"
  [io.github.sahzudin.omarchy-google-search]="https://github.com/sahzudin/omarchy-search.git"
  [krall.switchboard]="https://github.com/krall12/omarchy-switchboard.git"
)

add_plugin() {
  local id="$1" url="$2" enable="${3:-false}"
  if [[ -d "$PLUGINS_DIR/$id" ]]; then
    echo "already installed: $id"
    return
  fi
  local args=(plugin add "$url" --yes)
  [[ $enable == true ]] && args+=(--enable)
  omarchy "${args[@]}"
}

for id in "${!ENABLED_PLUGINS[@]}"; do
  add_plugin "$id" "${ENABLED_PLUGINS[$id]}" true
done

for id in "${!DISABLED_PLUGINS[@]}"; do
  add_plugin "$id" "${DISABLED_PLUGINS[$id]}"
done

for dir in "$BACKUP_DIR"/*/; do
  [[ -d $dir ]] || continue
  id=$(basename "$dir")
  if [[ ! -e "$PLUGINS_DIR/$id" ]]; then
    echo "restoring custom plugin: $id"
    cp -r "$dir" "$PLUGINS_DIR/$id"
  else
    echo "already present: $id"
  fi
done

omarchy-shell shell rescanPlugins >/dev/null
echo "All Omarchy plugins are installed."
