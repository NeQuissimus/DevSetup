{ config, pkgs, ... }: {
  boot.extraModprobeConfig = ''
    options zfs zfs_prefetch_disable=1
    options zfs zfs_arc_min=1073741824
    options zfs zfs_arc_max=2147483648
  '';

  services = {
    sanoid = {
      templates.backup = {
        daily = 14;
        monthly = 3;
        yearly = 3;
        autoprune = true;
        autosnap = true;
      };
    };

    zfs = {
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
  };

  virtualisation.docker = { storageDriver = "zfs"; };
}
