{ inputs, pkgs, ... }:
{
  imports = [
    ./brave.nix
    ./generic.nix
    ./git.nix
    ./hyprland.nix
    ./kitty.nix
    ./vscodium.nix
    ./zellij.nix
    ./zsh.nix
  ];
}
