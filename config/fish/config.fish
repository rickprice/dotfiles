if status is-interactive
    # Commands to run in interactive sessions can go here
    fish_vi_key_bindings
    fzf_configure_bindings --variables=\e\cV
    starship init fish | source
end
# Haskell Cabal Setup
set -xg PATH "/home/fprice/.cabal/bin:$PATH"
# Haskell Cabal Setup
# Haskell CHCUP Setup
set -xg PATH "/home/fprice/.ghcup/bin:$PATH"
# Haskell CHCUP Setup
# -- START ACTIVESTATE INSTALLATION
set -xg PATH "/home/fprice/.local/ActiveState/StateTool/beta/bin:$PATH"
if test ! -z "$ACTIVESTATE_ACTIVATED"; test -f "$ACTIVESTATE_ACTIVATED/activestate.yaml"
  echo "State Tool is operating on project $ACTIVESTATE_ACTIVATED_NAMESPACE, located at $ACTIVESTATE_ACTIVATED"
end
# -- STOP ACTIVESTATE INSTALLATION
# -- START ACTIVESTATE DEFAULT RUNTIME ENVIRONMENT
set -xg PATH "/home/fprice/.cache/activestate/bin:$PATH"
if test ! -z "$ACTIVESTATE_ACTIVATED"; test -f "$ACTIVESTATE_ACTIVATED/activestate.yaml"
  echo "State Tool is operating on project $ACTIVESTATE_ACTIVATED_NAMESPACE, located at $ACTIVESTATE_ACTIVATED"
end
# -- STOP ACTIVESTATE DEFAULT RUNTIME ENVIRONMENT
