{ config, pkgs, ... }:
{
  boot.extraModprobeConfig = ''
    options zfs zfs_prefetch_disable=1
    options zfs zfs_arc_min=1073741824
    options zfs zfs_arc_max=2147483648
  '';

  boot.zfs = {
    enableUnstable = true;
    forceImportRoot = false;
    forceImportAll = false;
    requestEncryptionCredentials = true;
  };

  services.zfs = {
    autoScrub = {
      enable = true;
      interval = "weekly";
    };

    autoSnapshot = {
      enable = false;
      flags = "-k -p --utc";
    };

    trim = {
      enable = true;
      interval = "weekly";
    };
  };

  virtualisation.docker = {
    storageDriver = "zfs";
  };
}
