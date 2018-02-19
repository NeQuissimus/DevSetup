{ config, pkgs, ... }:
{
  # remove this after 1st boot
  # see https://nixos.org/nixos/options.html#boot.zfs.forceimportroot
  # boot.kernelParams = ["zfs_force=1"];
 
  boot.initrd.kernelModules = [
    "icp"
    "spl"
    "zavl"
    "zcommon"
    "zfs"
    "znvpair"
    "zunicode"
  ];

  boot.zfs = {
    enableUnstable = true;
    forceImportRoot = false;
    forceImportAll = false;
  };

  boot.supportedFilesystems = [ "zfs" ];

  networking.hostId = "1cfca9bf";

  services.zfs = {
    autoScrub.enable = true;
    autoSnapshot = {
      enable = true;
      flags = "-k -p --utc";
    };
  };

  virtualization.docker = {
    storageDriver = "zfs";
    extraOptions = "--storage-opt=zfs.fsname=zroot/docker --userns-remap=docker";
  };
}
