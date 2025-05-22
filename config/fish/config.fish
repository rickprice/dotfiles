if status is-interactive
    # Commands to run in interactive sessions can go here
    fish_vi_key_bindings
    fzf_configure_bindings --variables=\e\cV
    starship init fish | source
    #
    # pyenv virtualenv-init - | source
    # Atuin
    atuin init fish | source
end
# Haskell Cabal Setup
set -xg PATH "/home/fprice/.cabal/bin:$PATH"
# Haskell Cabal Setup
# Haskell CHCUP Setup
set -xg PATH "/home/fprice/.ghcup/bin:$PATH"
# Haskell CHCUP Setup

# # Setup Python pyenv automatically
# pyenv init - | source
