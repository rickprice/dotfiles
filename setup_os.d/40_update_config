#! /bin/sh

# Fail script if any command fails
set -e

## keep track of the last executed command
trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
## echo an error message before exiting
trap 'echo "\"${last_command}\" command failed with exit code $?."' EXIT

# Need to remove any existing .config directory or stow won't work
echo "Moving .config to .config.backup to get it out of the way..."
rm -rf ~/.config.backup
mv ~/.config ~/.config.backup || true

rm -rf ~/.config/mimeapps.list
stow --dotfiles config
stow --dotfiles local
stow --dotfiles tmux
stow --dotfiles mutt
stow --dotfiles weechat
stow --dotfiles ssh
