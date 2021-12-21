function nnvim --wraps='XDG_CONFIG_HOME=~/.dotfiles/config.new nvim' --description 'alias nnvim=XDG_CONFIG_HOME=~/.dotfiles/config.new nvim'
  XDG_CONFIG_HOME=~/.dotfiles/config.new nvim $argv; 
end
