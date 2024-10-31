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
    etc."sysconfig/lm_sensors".text = ''
      HWMON_MODULES="coretemp"
    '';

    systemPackages = with pkgs; [ ];
  };

  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableAllFirmware = true;

  i18n = { defaultLocale = "en_US.UTF-8"; };

  networking = {
    defaultGateway = "10.0.0.2";

    firewall = {
      allowPing = true;
      allowedTCPPorts = [
        111
        139
        445
        2049
        4000
        4001
        4002
        20048
        config.services.prometheus.exporters.node.port
        config.services.prometheus.exporters.zfs.port
      ];
      allowedUDPPorts = [
        111
        149
        445
        2049
        4000
        4001
        4002
        20048
        config.services.prometheus.exporters.node.port
        config.services.prometheus.exporters.zfs.port
      ];
      enable = true;
      extraCommands = ''
        iptables -A INPUT -p tcp -s 10.0.0.0/8 --dport 548 -m conntrack --ctstate NEW,ESTABLISHED -j ACCEPT
      '';
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
        "0 17 * * 0 root reboot"
        "0 18 * * * root ${pkgs.google-cloud-sdk}/bin/gsutil -m rsync -c -r /nfs/pictures gs://nequi-nas-p/"
      ];
    };

    nfs.server = {
      enable = true;
      exports = ''
        /nfs 10.0.10.5(rw,fsid=0,no_subtree_check,insecure)
        /nfs/pictures 10.0.10.5(rw,nohide,no_subtree_check,insecure,sync,all_squash,anonuid=1000,anongid=100)
      '';
      lockdPort = 4001;
      mountdPort = 4002;
      statdPort = 4000;
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

    samba = {
      enable = true;
      openFirewall = true;
      securityType = "user";
      extraConfig = ''
        #        workgroup = WORKGROUP
                server string = smbnix
                netbios name = smbnix
                security = user
                hosts allow = 10.0.10.5 10.0.10.6 127.0.0.1 localhost
                hosts deny = 0.0.0.0/0
                guest account = nobody
                map to guest = bad user
                unix password sync = yes
                ea support = yes
                fruit:metadata = stream
                fruit:model = MacSamba
                fruit:veto_appledouble = no
                fruit:posix_rename = yes
                fruit:zero_file_id = yes
                fruit:wipe_intentionally_left_blank_rfork = yes
                fruit:delete_empty_adfiles = yes
                min protocol = SMB2
                vfs objects = fruit streams_xattr
                veto files = /._*/.DS_Store/
                delete veto files = yes
      '';
      shares = {
        pictures = {
          path = "/home/Shares/Pictures";
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "no";
          "create mask" = "0644";
          "directory mask" = "0755";
          "force user" = "nequi";
          "force group" = "users";
          "valid users" = "nequi sarah";
        };

        tv = {
          path = "/home/Shares/TV";
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "no";
          "create mask" = "0644";
          "directory mask" = "0755";
          "force user" = "nequi";
          "force group" = "users";
          "valid users" = "nequi";
        };
      };
    };

    smartd.enable = true;
  };

  services.logrotate.checkConfig = false;

  system = {
    autoUpgrade = {
      channel = "https://nixos.org/channels/nixos-24.05";
      dates = "15:00";
      enable = true;
    };
  };

  systemd.tmpfiles.rules = [ "d /etc/gcs 0700 root root" ];

  time.timeZone = "America/Toronto";
}
