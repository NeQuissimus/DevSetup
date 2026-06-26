# NixOS configuration

This repository is a Nix flake exposing NixOS configurations for each host.

## Machines

- `c220`       = Cisco C220
- `opi5plus`   = Orange Pi 5 Plus
- `rpi4b`      = Raspberry Pi 4B
- `supermicro` = Supermicro A1SRM-2758F
- `topton`     = Topton N8 N100

## Usage

```sh
sudo nixos-rebuild switch --flake .#<host>
```
