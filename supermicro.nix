{ config, pkgs, lib, ... }:

{
  imports = [
    ./supermicro-hardware.nix

    ./nixos/kernel.nix
    ./nixos/nix.nix
    ./nixos/security.nix
    ./nixos/ssh.nix
    ./nixos/users.nix
    ./nixos/zfs.nix
    ./nixos/zsh.nix
  ];

  boot = {
    kernelModules = [ "coretemp" ];
    kernelParams = [ "libata.force=noncq" ];

    loader = {
      grub = {
        device = "/dev/disk/by-id/usb-SABRENT_SABRENT_DB9876543214E-0:0";
        enable = true;
      };
    };

    tmp.cleanOnBoot = true;

    zfs = {
      forceImportAll = lib.mkForce false;
      forceImportRoot = false;
      requestEncryptionCredentials = lib.mkForce false;
    };
  };

  console.keyMap = "us";

  documentation.nixos.enable = false;

  environment = {
    etc."fuse.conf".text = ''
      user_allow_other
    '';

    etc."sysconfig/lm_sensors".text = ''
      HWMON_MODULES="coretemp"
    '';

    systemPackages = with pkgs; [ htop ];
  };

  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableAllFirmware = true;

  i18n = { defaultLocale = "en_US.UTF-8"; };

  networking = {
    defaultGateway = "10.0.0.2";

    firewall = {
      allowPing = true;
      enable = true;
    };

    hostName = "supermicro";
    hostId = "123b567b";
  };

  powerManagement = {
    cpuFreqGovernor = "powersave";
    powertop.enable = true;
  };

  services = {
    cron = {
      enable = true;
      systemCronJobs = [
        "0 17 * * 5 root reboot"
        ''
          0 18 10 */3 5 root GOOGLE_APPLICATION_CREDENTIALS=/etc/gcs/serviceaccount.json ${pkgs.google-cloud-sdk}/bin/gcloud storage rsync "/tank/immich_enc" "gs://nequi-nas-i/" --recursive --delete-unmatched-destination-objects''
      ];
    };

    immich = {
      enable = true;
      host = "0.0.0.0";
      mediaLocation = "/mnt/immich";
      openFirewall = true;
    };

    jellyfin = {
      enable = true;
      openFirewall = true;
    };

    ntp = {
      enable = true;
      servers = [
        "0.ca.pool.ntp.org"
        "1.ca.pool.ntp.org"
        "2.ca.pool.ntp.org"
        "3.ca.pool.ntp.org"
      ];
    };

    thermald.enable = true;

    openssh.enable = true;

    syslogd = {
      defaultConfig = "*.* @10.0.0.52:5514";

      enable = true;
    };

    smartd.enable = true;
  };

  services.logrotate.checkConfig = false;

  systemd = {
    services.immich_enc = {
      after = [ "tank.mount" ];
      before = [ "immich-server.service" "immich-machine-learning.service" ];

      description = "Mount immich volume";
      path = with pkgs; [ util-linux ];

      serviceConfig = {
        ExecStart = ''
          ${
            lib.getBin pkgs.gocryptfs
          }/bin/gocryptfs -passfile /etc/gocryptfs -allow_other "/tank/immich_enc" "/mnt/immich"'';

        Type = "forking";
      };

      requiredBy =
        [ "immich-server.service" "immich-machine-learning.service" ];
    };

    tmpfiles.rules = [ "d /etc/gcs 0700 root root" ];
  };

  time.timeZone = "America/Toronto";
}
