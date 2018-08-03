{ config, lib, pkgs, ... }:

{
  programs.zsh.autosuggestions.enable = true;
  
  system.stateVersion = "18.09";
}
