function onvim --wraps='XDG_CONFIG_HOME=~/.dotfiles/config.old nvim' --description 'alias onvim=XDG_CONFIG_HOME=~/.dotfiles/config.old nvim'
  XDG_CONFIG_HOME=~/.dotfiles/config.old nvim $argv; 
end
