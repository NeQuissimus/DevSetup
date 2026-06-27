{ inputs, pkgs, ... }:
{
  imports = [
    ./generic.nix
    ./git.nix
    ./hyprland.nix
    ./kitty.nix
    ./vscodium.nix
    ./zellij.nix
    ./zen-browser.nix
    ./zsh.nix
  ];
}
