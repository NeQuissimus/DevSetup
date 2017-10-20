{ config, lib, pkgs, ... }: rec {
   config.services.xserver = {
    autorun = true;

    displayManager = {
      sddm = {
        enable = true;
      };
    };

    enable = true;

    windowManager.sway = {
      enable = true;
    };

    xkbOptions = "ctrl:nocaps";
  };
}
