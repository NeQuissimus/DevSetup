{ config, pkgs, lib, ... }:
let
  dnsmasq_filters_path = "/etc/dnsmasq/filters";

  dnsmasq_filters = {
    abuse_ch = "https://urlhaus.abuse.ch/downloads/hostfile/";
    disconnectme_ad =
      "https://s3.amazonaws.com/lists.disconnect.me/simple_ad.txt";
    disconnectme_tracking =
      "https://s3.amazonaws.com/lists.disconnect.me/simple_tracking.txt";
    energized_regional =
      "https://block.energized.pro/extensions/regional/formats/hosts";
    energized_unified = "https://block.energized.pro/unified/formats/hosts";
    energized_xtreme =
      "https://block.energized.pro/extensions/xtreme/formats/hosts";
    notracking =
      "https://raw.githubusercontent.com/notracking/hosts-blocklists/master/hostnames.txt";
    steven_black =
      "https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews-gambling-porn/hosts";
  };
in {
  imports = [ ./hardware-configuration.nix ./nixos/docker.nix ];

  boot = {
    cleanTmpDir = true;

    loader = {
      grub = {
        device = "/dev/sda";
        enable = true;
        version = 2;
      };
    };

    kernel = {
      sysctl = {
        "vm.drop_caches" = 1;
        "vm.swappiness" = 1;
      };
    };

    kernelPackages = pkgs.linuxPackages_hardened;
    kernelParams = [ "acpi=off" ];
  };

  console.keyMap = "us";

  documentation.nixos.enable = false;

  environment = { systemPackages = with pkgs; [ certbot inetutils screen ]; };

  i18n = { defaultLocale = "en_US.UTF-8"; };

  networking = {
    firewall = {
      allowPing = false;
      allowedTCPPorts = [ 22 53 8086 8888 ];
      allowedUDPPorts = [ 53 ];
      enable = true;
    };

    hostName = "hape";
  };

  nix = {
    binaryCaches = [ "https://cache.nixos.org" ];
    buildCores = 8;

    extraOptions = ''
      auto-optimise-store = true
      binary-caches-parallel-connections = 3
      connect-timeout = 10
    '';

    gc = {
      automatic = true;
      dates = "18:00";
      options = "--delete-older-than 30";
    };

    maxJobs = 4;
    nrBuildUsers = 30;

    optimise = {
      automatic = true;
      dates = [ "18:00" ];
    };

    trustedBinaryCaches = [ "https://cache.nixos.org" ];
    useSandbox = true;
  };

  nixpkgs = { config = { allowUnfree = true; }; };

  security = { hideProcessInformation = true; };

  services = {
    cron = {
      enable = true;
      systemCronJobs = [
        "0 0 * * * root reboot"
        "* * * * * nequi /home/nequi/stats/mikrotik-stats/start.sh"
      ];
    };

    dnscrypt-proxy2 = {
      enable = true;

      settings = {
        cache = true;
        cache_max_ttl = 86400;
        cache_size = 1024;
        cert_refresh_delay = 240;
        doh_servers = true;
        fallback_resolver = "1.1.1.1:53";
        ignore_system_dns = true;
        listen_addresses = [ "127.0.0.1:5300" "[::1]:5300" ];
        log_files_max_size = 10;
        keepalive = 30;
        require_dnssec = true;
        require_nofilter = true;
        require_nolog = true;
        server_names = [ "cloudflare" ];
        timeout = 3000;

        sources.public-resolvers = {
          urls = [
            "https://raw.githubusercontent.com/DNSCrypt/dnscrypt-resolvers/master/v3/public-resolvers.md"
            "https://download.dnscrypt.info/resolvers-list/v3/public-resolvers.md"
          ];
          minisign_key =
            "RWQf6LRCGA9i53mlYecO4IzT51TGPpvWucNSCh1CBM0QTaLn73Y7GFO3";
          cache_file = "public-resolvers.md";
        };

      };

    };

    dnsmasq = {
      alwaysKeepRunning = true;
      enable = true;
      extraConfig = ''
        no-hosts
        no-negcache
        no-resolv
        hostsdir=${dnsmasq_filters_path}
        all-servers
        cache-size=2000
        local-ttl=3600
        min-cache-ttl=3600
      '';
      servers = [ "127.0.0.1#5300" ];
    };

    influxdb.enable = true;

    locate = { enable = true; };

    logind.extraConfig = "HandleLidSwitch=ignore";

    ntp = {
      enable = true;
      servers = [
        "0.ca.pool.ntp.org"
        "1.ca.pool.ntp.org"
        "2.ca.pool.ntp.org"
        "3.ca.pool.ntp.org"
      ];
    };

    openssh = {
      enable = true;
      passwordAuthentication = true; # FIX
      permitRootLogin = "yes"; # FIX
    };

    prometheus.exporters.dnsmasq = {
      enable = true;
      leasesPath = "/var/lib/dnsmasq/dnsmasq.leases";
    };
  };

  system = {
    autoUpgrade = {
      channel = "https://nixos.org/channels/nixos-20.09";
      dates = "19:00";
      enable = true;
    };

    stateVersion = "20.09";
  };

  systemd.services.dnsblocklists = {
    enable = true;
    description = "Download updated blacklists";
    before = [ "dnsmasq.service" ];
    startAt = "hourly";

    path = with pkgs; [ curl ];
    script = ''
      rm -rf ${dnsmasq_filters_path}
      mkdir -p ${dnsmasq_filters_path}
    '' + builtins.concatStringsSep "\n" (lib.mapAttrsToList
      (key: value: "curl -sSL ${value} | sed 's|^0.0.0.0 ||g' | sed 's|^127.0.0.1 ||g' | sed 's|^[^#]|0.0.0.0 |g' > ${dnsmasq_filters_path}/${key}")
      dnsmasq_filters);
  };

  time = { timeZone = "America/Toronto"; };

  users = {
    extraUsers = {
      nequi = {
        createHome = true;
        extraGroups = [ "docker" ];
        group = "users";
        home = "/home/nequi";
        name = "nequi";

        openssh = {
          authorizedKeys = {
            keys = [
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGvyuou95vwSRLc9p8NM/2djOXBrXu7tAF9VxGI8H8GV"
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKQiX5oeTMBkaFLxsGoxLGXEOExB/ND6dgyewKurDZD4"
            ];
          };
        };

        useDefaultShell = true;
        uid = 1000;
      };
    };
  };

  virtualisation.docker = {
    enable = true;
    enableOnBoot = true;
  };
}
