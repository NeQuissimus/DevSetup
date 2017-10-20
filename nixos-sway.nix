{ config, lib, pkgs, ... }: rec {
   config.services.xserver = {
    autorun = true;
    defaultDepth = 24;

    displayManager = {
      gdm = {
        enable = true;
      };
    };

    enable = true;
    exportConfiguration = true;

    synaptics = {
      enable = true;
      tapButtons = false;
      twoFingerScroll = true;
    };

    windowManager.sway = {
      enable = true;
    };

    xkbOptions = "ctrl:nocaps";
  };
}
