{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware.nix

    ../../nixos/dns.nix
    ../../nixos/docker.nix
    ../../nixos/kernel.nix
    ../../nixos/nix.nix
    ../../nixos/security.nix
    ../../nixos/ssh.nix
    ../../nixos/users.nix
    ../../nixos/zfs.nix
    ../../nixos/zsh.nix
  ];

  boot = {
    kernelModules = [ "coretemp" ];

    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
    };
  };

  console.keyMap = "us";

  documentation.nixos.enable = false;

  environment = {
    etc = {
      "fuse.conf".text = ''
        user_allow_other
      '';

      "machine-id".source = "/nix/persist/etc/machine-id";
      "gocryptfs".source = "/nix/persist/etc/gocryptfs";

      "ssh/ssh_host_rsa_key".source = "/nix/persist/etc/ssh/ssh_host_rsa_key";
      "ssh/ssh_host_rsa_key.pub".source =
        "/nix/persist/etc/ssh/ssh_host_rsa_key.pub";
      "ssh/ssh_host_ed25519_key".source =
        "/nix/persist/etc/ssh/ssh_host_ed25519_key";
      "ssh/ssh_host_ed25519_key.pub".source =
        "/nix/persist/etc/ssh/ssh_host_ed25519_key.pub";

      "sysconfig/lm_sensors".text = ''
        HWMON_MODULES="coretemp"
      '';
    };
  };

  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableAllFirmware = true;

  i18n = { defaultLocale = "en_US.UTF-8"; };

  networking = {
    defaultGateway = "10.0.0.2";

    firewall = {
      allowPing = true;

      allowedTCPPorts = [
        8080 # MediaWiki
      ];

      allowedUDPPorts = [ ];

      enable = true;
    };

    hostName = "topton";
    hostId = "a1b204e6";

    interfaces."enp4s0" = {
      ipv4.addresses = [{
        address = "10.102.0.37";
        prefixLength = 16;
      }];
      ipv6.addresses = [{
        address = "fd00:1873::200";
        prefixLength = 117;
      }];
    };
  };

  powerManagement = {
    cpuFreqGovernor = "powersave";
    powertop.enable = true;
  };

  programs.htop.enable = true;

  services = {
    cron = {
      enable = true;
      systemCronJobs = [
        "0 17 * * 5 root reboot"
        ''
          0 18 10 * * root GOOGLE_APPLICATION_CREDENTIALS=/etc/gcs/serviceaccount.json ${pkgs.google-cloud-sdk}/bin/gcloud storage rsync "/tank/immich_enc" "gs://nequi-nas-i/" --recursive --delete-unmatched-destination-objects''
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

    mediawiki = {
      enable = false;

      extraConfig = ''
        $wgReadOnly = "Locked";
      '';

      httpd.virtualHost.listen = [{
        ip = "127.0.0.1";
        port = 8080;
        ssl = false;
      }];

      name = "Wikipedia dump";
      passwordFile = pkgs.writeText "password" "admin";
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

    sanoid = {
      datasets."tank/immich_enc".useTemplate = [ "backup" ];
      enable = true;
    };

    smartd.enable = true;

    syslogd = {
      defaultConfig = "*.* @10.0.0.52:5514";

      enable = true;
    };
  };

  services.logrotate.checkConfig = false;

  systemd = {
    services.immich_enc = {
      after = [ "tank-immich_enc.mount" ];
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

    tmpfiles.rules =
      [ "d /mnt/immich 0700 immich immich" "d /etc/gcs 0700 root root" ];
  };

  time.timeZone = "America/Toronto";
}
