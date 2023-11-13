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

# SSH
DIR_TO_COPY=~/.ssh
rsync --archive $DIR_TO_COPY $EXPORT_DIRECTORY

# GPG
DIR_TO_COPY=~/.gnupg
rsync --archive  $DIR_TO_COPY $EXPORT_DIRECTORY

# Dotfiles
DIR_TO_COPY=~/.dotfiles/.dotter
rsync --archive  $DIR_TO_COPY $EXPORT_DIRECTORY

# Fish
DIR_TO_COPY=~/.local/share/fish
rsync --archive  $DIR_TO_COPY $EXPORT_DIRECTORY

# WireGuard
DIR_TO_COPY=~/WireGuard
rsync --archive  $DIR_TO_COPY $EXPORT_DIRECTORY

# Zoxzide
DIR_TO_COPY=~/.local/share/zoxide
rsync --archive  $DIR_TO_COPY $EXPORT_DIRECTORY

# Darktable
DIR_TO_COPY=~/.config/darktable
rsync --archive  $DIR_TO_COPY $EXPORT_DIRECTORY
DIR_TO_COPY=~/Documents/Personal/DarktablePersonal
rsync --archive  $DIR_TO_COPY $EXPORT_DIRECTORY
DIR_TO_COPY=~/Documents/Personal/DarktableCommercial
rsync --archive  $DIR_TO_COPY $EXPORT_DIRECTORY

# Dropbox
DIR_TO_COPY=~/Documents/Personal/Dropbox
rsync --archive  $DIR_TO_COPY $EXPORT_DIRECTORY
