# dotfiles

Personal dotfiles managed with [Dotter](https://github.com/SuperCuber/dotter).

## Branches

| Branch | User |
|--------|------|
| `main` | Frederick |
| `tamara_price` | Tamara |

## What's configured

| Package | Config |
|---------|--------|
| XMonad | `config/xmonad/xmonad.hs` |
| xmobar | `config/xmobar/xmobarrc` / `home/xmobarrc` |
| i3 | `config/i3/config`, `config/i3status/config` |
| Neovim | `config/nvim/` |
| WezTerm | `config/wezterm/wezterm.lua` |
| Alacritty | `config/alacritty/alacritty.yml` |
| Fish | `config/fish/` |
| Picom | `config/picom/picom.conf` |
| Dunst | `config/dunst/dunstrc` |
| PipeWire | `config/pipewire/pipewire-pulse.conf` |
| Autorandr | `config/autorandr/` |
| LightDM | `etc/lightdm/` |
| Scripts | `local/bin/` |

## Deploying

Install [Dotter](https://github.com/SuperCuber/dotter), then:

```sh
# Deploy all packages listed in .dotter/local.toml
dotter deploy
```

Edit `.dotter/local.toml` to select which packages to deploy on the current machine.
