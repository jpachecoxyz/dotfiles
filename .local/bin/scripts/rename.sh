#!/usr/bin/env bash

set -euo pipefail

# 2. Elegir archivos (multi-select)
FILES=$(ls -1 | fzf -m --prompt="Select files to rename: ")

[[ -z "$FILES" ]] && exit 0

i=1
while IFS= read -r file; do
  [[ -f "$file" ]] || continue

  ext=""
  if [[ "$file" == *.* ]]; then
    ext=".${file##*.}"
  fi

  new_name="${i}${ext}"

  # Evitar sobreescritura
  if [[ -e "$new_name" ]]; then
    echo "Skipping $file → $new_name (already exists)"
  else
    mv -- "$file" "$new_name"
    echo "$file → $new_name"
  fi

  ((i++))
done <<< "$FILES"
