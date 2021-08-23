{ config, pkgs, lib, ... }:
let
  dnsmasq_filters_path = "/etc/dnsmasq/filters";

  dnsmasq_filters = {
    abuse_ch = "https://urlhaus.abuse.ch/downloads/hostfile/";
    disconnectme_ad =
      "https://s3.amazonaws.com/lists.disconnect.me/simple_ad.txt";
    disconnectme_tracking =
      "https://s3.amazonaws.com/lists.disconnect.me/simple_tracking.txt";
    notracking =
      "https://raw.githubusercontent.com/notracking/hosts-blocklists/master/hostnames.txt";
    steven_black =
      "https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews-gambling-porn/hosts";
  };

  ip_router = "10.0.0.2";
  ip_ap = "10.0.0.3";
  ip_imac_wifi = "10.0.10.1";
  ip_imac_eth = "10.0.10.2";
  ip_nas = "10.0.0.33";
  ip_pine = "10.0.10.10";
  ip_raspi4 = "10.0.10.38";
  ip_ux305c = "10.0.10.3";
in {
  imports = [ ./6910p-hardware.nix ./6910p-secrets.nix ./extras/6910p-dhcp.nix ];

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

  environment = {
    systemPackages = with pkgs; [ ];
  };

  i18n = { defaultLocale = "en_US.UTF-8"; };

  networking = {
    defaultGateway = "10.0.0.2";

    firewall = {
      allowPing = false;
      allowedTCPPorts = [];
      allowedUDPPorts = [];
      enable = true;
      extraCommands = ''
        iptables -A INPUT -p tcp -s 10.0.0.0/8 --dport ${toString dnsmasq_port} -m conntrack --ctstate NEW,ESTABLISHED -j ACCEPT
        iptables -A INPUT -p udp -s 10.0.0.0/8 --dport ${toString dnsmasq_port} -m udp -j ACCEPT

        iptables -A INPUT -p tcp -s ${ip_ux305c}/32 --dport ${toString (builtins.head config.services.openssh.ports)} -m conntrack --ctstate NEW,ESTABLISHED -j ACCEPT
      '';
    };

    hostName = "hape";

    interfaces."enp0s25" = {
      ipv4.addresses = [{
        address = "10.0.10.26";
        prefixLength = 16;
      }];
      ipv6.addresses = [{
        address = "fd00:1872::100";
        prefixLength = 117;
      }];
    };

    nameservers = ["127.0.0.1" "9.9.9.9"];
    useDHCP = false;
    usePredictableInterfaceNames = true;
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

  services = {
    cron = {
      enable = true;
      systemCronJobs = [
        "0 0 * * * root reboot"
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
        addn-hosts=${dnsmasq_filters_path}
        all-servers
        cache-size=2000
        local-ttl=3600
        min-cache-ttl=3600
        port=${toString dnsmasq_port}
      '';
      servers = [ "127.0.0.1#5300" ];
    };

    locate.enable = true;

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
      passwordAuthentication = false;
      permitRootLogin = "no";
    };
  };

  system = {
    autoUpgrade = {
      channel = "https://nixos.org/channels/nixos-21.05";
      dates = "19:00";
      enable = true;
    };

    stateVersion = "21.05";
  };

  systemd.services.dnsblocklists = {
    enable = true;
    description = "Download updated blacklists";
    before = [ "dnsmasq.service" ];
    startAt = "hourly";

    path = with pkgs; [ curl ];
    script = builtins.concatStringsSep "\n" (lib.mapAttrsToList (key: value:
      "curl -sSL ${value} | sed 's|^0.0.0.0 ||g' | sed 's|^127.0.0.1 ||g' | sed 's|^\\s*#.*||g' | sed 's|^\\s*::.*||g' | sed 's|.*shopify.*||g' | sed 's|^|0.0.0.0 |g' >> ${dnsmasq_filters_path}.tmp")
      dnsmasq_filters) + ''

        echo '${ip_router} router.nequissimus.com' >> ${dnsmasq_filters_path}.tmp
        echo '${ip_ap} ap.nequissimus.com' >> ${dnsmasq_filters_path}.tmp
        echo '${ip_nas} nas.nequissimus.com' >> ${dnsmasq_filters_path}.tmp
        echo '${ip_pine} pine.nequissimus.com' >> ${dnsmasq_filters_path}.tmp

        cat ${dnsmasq_filters_path}.tmp | sort -u > ${dnsmasq_filters_path}
        rm -rf ${dnsmasq_filters_path}.tmp
      '';
  };

  systemd.services.journalbeat.serviceConfig.RestartSec = "30";

  time.timeZone = "America/Toronto";

  users = {
    extraUsers = {
      nequi = {
        createHome = true;
        extraGroups = [ "docker" ];
        group = "users";
        home = "/home/nequi";
        isNormalUser = true;
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
}
