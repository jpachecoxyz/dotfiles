#!/usr/bin/env bash

IFS='
'
declare -A map
for item in `lpass ls --format "%ai %an %au"`;do
	id=${item#* }
	id="${id#"${id%%[![:space:]]*}"}"
	id="${id%"${id##*[![:space:]]}"}"

	if [ "$id" != "" ]; then
		map[$id]=${item%% *}
	fi
done

selection=`printf '%s\n' "${!map[@]}" | tofi --prompt 'Account'`
if [ "$selection" != "" ]; then
    lpass show -c --password ${map[$selection]} &
fi
