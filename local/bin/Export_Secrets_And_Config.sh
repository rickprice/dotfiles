#! /bin/bash

# Fail script if any command fails
set -e

## keep track of the last executed command
trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
## echo an error message before exiting
trap 'echo "\"${last_command}\" command failed with exit code $?."' EXIT

EXPORT_DIRECTORY=/run/media/fprice/KINGSTON/FPRICE_Secrets_And_Config

echo "Exporting to:" $EXPORT_DIRECTORY

mkdir -p $EXPORT_DIRECTORY

# SSH
cp -Rp ~/.ssh $EXPORT_DIRECTORY

# GPG
cp -Rp ~/.gnupg $EXPORT_DIRECTORY

# Dotfiles
DIR_TO_COPY=.dotfiles/.dotter
mkdir -p $EXPORT_DIRECTORY/$DIR_TO_COPY
cp -Rp ~/$DIR_TO_COPY/secrets.toml $EXPORT_DIRECTORY/$DIR_TO_COPY/.

# Fish
DIR_TO_COPY=.local/share/fish
mkdir -p $EXPORT_DIRECTORY/$DIR_TO_COPY
cp -Rp ~/$DIR_TO_COPY/fish_history $EXPORT_DIRECTORY/$DIR_TO_COPY/.

# Python Version Requirements Cache
DIR_TO_COPY=.PythonVersionRequirementsCache
mkdir -p $EXPORT_DIRECTORY/$DIR_TO_COPY
cp -Rp ~/$DIR_TO_COPY/* $EXPORT_DIRECTORY/$DIR_TO_COPY/.

# WireGuard
DIR_TO_COPY=WireGuard
mkdir -p $EXPORT_DIRECTORY/$DIR_TO_COPY
cp -Rp ~/$DIR_TO_COPY/* $EXPORT_DIRECTORY/$DIR_TO_COPY/.

# ActiveState.yaml
DIR_TO_COPY=TheHomeRepot
mkdir -p $EXPORT_DIRECTORY/$DIR_TO_COPY
cp -Rp ~/$DIR_TO_COPY/activestate.yaml.fish_fixes $EXPORT_DIRECTORY/$DIR_TO_COPY/.

# Zoxzide
DIR_TO_COPY=.local/share/zoxide
mkdir -p $EXPORT_DIRECTORY/$DIR_TO_COPY
cp -Rp ~/$DIR_TO_COPY/db.zo $EXPORT_DIRECTORY/$DIR_TO_COPY/.

# Darktable
DIR_TO_COPY=.config/darktable
mkdir -p $EXPORT_DIRECTORY/$DIR_TO_COPY
cp -Rp ~/$DIR_TO_COPY/* $EXPORT_DIRECTORY/$DIR_TO_COPY/.

# Pulse
DIR_TO_COPY=etc/pulse
mkdir -p $EXPORT_DIRECTORY/$DIR_TO_COPY
cp -Rp /$DIR_TO_COPY/* $EXPORT_DIRECTORY/$DIR_TO_COPY/.

