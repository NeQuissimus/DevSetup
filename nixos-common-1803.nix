{ config, lib, pkgs, ... }:

{
  programs.zsh.enableAutosuggestions = true;
  
  system.stateVersion = "18.03";
}
