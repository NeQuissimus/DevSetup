{ config, lib, pkgs, ... }:

{
  boot = {
    loader = {
      efi.canTouchEfiVariables = true;

      systemd-boot = {
        editor = false;
        enable = true;
      };
    };

    plymouth = { enable = false; };

    supportedFilesystems = [ "exfat" "zfs" ];
  };
}
