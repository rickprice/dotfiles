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

## Notable behaviours

- Clicking the i3 bar (any area) launches/toggles `gsimplecal` — configured in `config/i3/config`
- `gsimplecal` calendar font is 150% of the GTK default — configured via `config/gtk-3.0/gtk.css`
- Clicking the xmobar date/time launches/toggles `gsimplecal` — configured in `home/xmobarrc`

## Deploying

Install [Dotter](https://github.com/SuperCuber/dotter), then:

```sh
# Deploy all packages listed in .dotter/local.toml
dotter deploy
```

Edit `.dotter/local.toml` to select which packages to deploy on the current machine.
