{ config, pkgs, ... }:
{
  boot.extraModprobeConfig = ''
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
      interval = "daily";
    };

    autoSnapshot = {
      enable = false;
      flags = "-k -p --utc";
    };

    trim = {
      enable = true;
      interval = "hourly";
    };
  };

  virtualisation.docker = {
    storageDriver = "zfs";
  };
}
