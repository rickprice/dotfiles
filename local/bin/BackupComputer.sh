#! /bin/bash

# Fail script if any command fails
set -e
set -x

## keep track of the last executed command
trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
## echo an error message before exiting
trap 'echo "\"${last_command}\" command failed with exit code $?."' EXIT

# EXPORT_DIRECTORY=/run/media/fprice/KINGSTON/FPRICE_Secrets_And_Config
EXPORT_DIRECTORY=/run/media/fprice/Backup

echo "Exporting to:" $EXPORT_DIRECTORY

mkdir -p $EXPORT_DIRECTORY

# Home
DIR_TO_COPY=/export/home
sudo rsync --archive $DIR_TO_COPY $EXPORT_DIRECTORY

# etc
DIR_TO_COPY=/etc
sudo rsync --archive  $DIR_TO_COPY $EXPORT_DIRECTORY
