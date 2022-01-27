if status is-interactive
    # Commands to run in interactive sessions can go here
    fish_vi_key_bindings
    fzf_configure_bindings --variables=\e\cV
    starship init fish | source
end
# -- START ACTIVESTATE INSTALLATION
set -xg PATH "/home/fprice/.local/ActiveState/StateTool/release:$PATH"
# -- STOP ACTIVESTATE INSTALLATION
# -- START ACTIVESTATE DEFAULT RUNTIME ENVIRONMENT
set -xg PATH "/home/fprice/.cache/activestate/bin:$PATH"
# -- STOP ACTIVESTATE DEFAULT RUNTIME ENVIRONMENT
