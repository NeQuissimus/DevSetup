{ config, pkgs, ... }:
{
  boot.zfs = {
    enableUnstable = true;
    forceImportRoot = false;
    forceImportAll = false;
    requestEncryptionCredentials = true;
  };

  services.zfs = {
    autoScrub.enable = true;
    autoSnapshot = {
      enable = true;
      flags = "-k -p --utc";
    };
  };

  virtualisation.docker = {
    storageDriver = "zfs";
  };
}
