{ config, lib, pkgs, ... }:

{
  system.autoUpgrade = {
    allowReboot = true;
    dates = "daily";
    enable = true;
    flake = lib.mkDefault "github:NeQuissimus/DevSetup#${config.networking.hostName}";
    operation = "boot";
  };

  security = {
    allowSimultaneousMultithreading = true;
    apparmor.enable = true;
    sudo = {
      enable = true;
      execWheelOnly = true;
      wheelNeedsPassword = true;
    };
  };
}
