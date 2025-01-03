#!/usr/bin/env bash

# Clear the temporary file
: > /tmp/ee-nnn.tmp

# Launch nnn with the -p option to write selected paths to the file
nnn -p /tmp/ee-nnn.tmp "$1"

# Print the contents of the selection file
cat /tmp/ee-nnn.tmp
