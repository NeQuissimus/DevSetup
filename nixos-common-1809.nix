{ config, lib, pkgs, ... }:

{
  programs.zsh.autosuggestions.enable = true;
  
  system.nixos.stateVersion = "18.09";
}
