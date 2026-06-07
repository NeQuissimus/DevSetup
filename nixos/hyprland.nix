{ config, lib, pkgs, ... }:

{
  programs = {
    hypridle.enable = true;

    hyprland = {
      enable = true;
      withUWSM = true;
      xwayland.enable = true;
    };

    hyprlock.enable = true;

    hyprsunset.enable = true;
  };

  services = {
    displayManager.sddm = {
      enable = true;
  
      wayland.enable = true;
    };
    
    xserver.enable = true;
  };
}