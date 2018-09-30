{ config, lib, pkgs, ... }:

{
  nixosManual.enable = false;

  programs.zsh.autosuggestions.enable = true;

  system.stateVersion = "18.09";
}
