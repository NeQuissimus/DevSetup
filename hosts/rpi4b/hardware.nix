{ config, lib, pkgs, modulesPath, ... }:

# sudo nix-channel --list
# nixos https://nixos.org/channels/nixos-24.11
# nixos-hardware https://github.com/NixOS/nixos-hardware/archive/master.tar.gz

{
  imports = [ <nixos-hardware/raspberry-pi/4> ];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = [ "noatime" ];
    };
  };

  hardware.enableRedistributableFirmware = true;
}
