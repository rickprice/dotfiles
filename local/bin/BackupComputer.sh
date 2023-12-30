#! /bin/bash

# Keep DropBox out of our hair
killall -q dropbox

# Fail script if any command fails
set -e
set -x

## keep track of the last executed command
trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
## echo an error message before exiting
trap 'echo "\"${last_command}\" command failed with exit code $?."' EXIT

# Get out of the way
cd /

# EXPORT_DIRECTORY=/run/media/fprice/KINGSTON/FPRICE_Secrets_And_Config
EXPORT_DIRECTORY=/run/media/fprice/Backup

echo "Exporting to:" $EXPORT_DIRECTORY

mkdir -p $EXPORT_DIRECTORY

# etc
DIR_TO_COPY=/etc
sudo rsync -aAXv $DIR_TO_COPY --delete $EXPORT_DIRECTORY

# Home
DIR_TO_COPY=/home
sudo rsync -aAXv $DIR_TO_COPY --delete $EXPORT_DIRECTORY

# Start DropBox back up again
dropbox &
