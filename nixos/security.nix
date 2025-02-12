{ config, lib, pkgs, ... }:

{
  system.autoUpgrade = {
    allowReboot = true;
    channel = lib.mkDefault "https://nixos.org/channels/nixos-24.11";
    dates = "daily";
    enable = true;
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
