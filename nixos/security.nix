{ config, lib, pkgs, ... }:

{
  system.autoUpgrade = {
    allowReboot = true;
    channel = lib.mkDefault "https://nixos.org/channels/nixos-24.11";
    dates = "15:00";
    enable = true;
    operation = "boot";
  };

  security = {
    allowSimultaneousMultithreading = true;
    apparmor.enable = true;
    sudo = {
      enable = true;
      wheelNeedsPassword = true;
    };
  };
}
