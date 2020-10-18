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
  imports = [ ./6910p-hardware.nix ./6910p-secrets.nix ./modules/telegraf.nix ./nixos/docker.nix ];

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
    etc."grafana/dashboards/dns.json".source = ./6910p-grafana/dns.json;
    etc."grafana/dashboards/wireless.json".source =
      ./6910p-grafana/wireless.json;

    systemPackages = with pkgs; [ certbot inetutils screen ];
  };

  i18n = { defaultLocale = "en_US.UTF-8"; };

  networking = {
    firewall = {
      allowPing = false;
      allowedTCPPorts = [ 22 53 config.services.grafana.port ];
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
        addn-hosts=${dnsmasq_filters_path}
        all-servers
        cache-size=2000
        local-ttl=3600
        min-cache-ttl=3600
      '';
      servers = [ "127.0.0.1#5300" ];
    };

    grafana = {
      addr = "";
      enable = true;
      provision = {
        enable = true;
        dashboards = [{ options.path = "/etc/grafana/dashboards/"; }];
        datasources = [{
          name = "InfluxDB";
          type = "influxdb";
          database = "nequissimus";
          editable = false;
          access = "proxy";
          url = ("http://localhost:8086");
        }];
      };
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
      passwordAuthentication = false;
      permitRootLogin = "no";
    };

    prometheus.exporters.dnsmasq = {
      enable = true;
      leasesPath = "/var/lib/dnsmasq/dnsmasq.leases";
    };

    telegraf = {
      enable = true;
      extraConfig = {
        inputs = {
          minecraft = {
            port = "25576";
            server = "10.0.10.38";
          };

          prometheus.urls = [
            "http://localhost:9436/metrics"
            "http://localhost:8080/"
            "http://localhost:9153"
          ];
        };

        outputs.influxdb = {
          database = "nequissimus";
          urls = [ "http://localhost:8086" ];
        };
      };
      rawConfig = ''
        [[inputs.snmp]]
          agents = [ "10.0.0.2:161", "10.0.0.3:161" ]

          [[inputs.snmp.field]]
            name = "hostname"
            oid = ".1.3.6.1.2.1.1.5.0"
            is_tag = true
          [[inputs.snmp.field]]
            name = "uptime"
            oid = ".1.3.6.1.2.1.1.3.0"
          [[inputs.snmp.field]]
            name = "cpu-frequency"
            oid = ".1.3.6.1.4.1.14988.1.1.3.14.0"
          [[inputs.snmp.field]]
            name = "cpu-load"
            oid = ".1.3.6.1.2.1.25.3.3.1.2.1"
          [[inputs.snmp.field]]
            name = "active-fan"
            oid = ".1.3.6.1.4.1.14988.1.1.3.9.0"
          [[inputs.snmp.field]]
            name = "voltage"
            oid = ".1.3.6.1.4.1.14988.1.1.3.8.0"
          [[inputs.snmp.field]]
            name = "temperature"
            oid = ".1.3.6.1.4.1.14988.1.1.3.10.0"
          [[inputs.snmp.field]]
            name = "processor-temperature"
            oid = ".1.3.6.1.4.1.14988.1.1.3.11.0"
          [[inputs.snmp.field]]
            name = "current"
            oid = ".1.3.6.1.4.1.14988.1.1.3.13.0"
          [[inputs.snmp.field]]
            name = "fan-speed"
            oid = ".1.3.6.1.4.1.14988.1.1.3.17.0"
          [[inputs.snmp.field]]
            name = "fan-speed2"
            oid = ".1.3.6.1.4.1.14988.1.1.3.18.0"
          [[inputs.snmp.field]]
            name = "power-consumption"
            oid = ".1.3.6.1.4.1.14988.1.1.3.12.0"
          [[inputs.snmp.field]]
            name = "psu1-state"
            oid = ".1.3.6.1.4.1.14988.1.1.3.15.0"
          [[inputs.snmp.field]]
            name = "psu2-state"
            oid = ".1.3.6.1.4.1.14988.1.1.3.16.0"

          # Interfaces
          [[inputs.snmp.table]]
            name = "snmp-interfaces"
            inherit_tags = ["hostname"]
            [[inputs.snmp.table.field]]
              name = "if-name"
              oid = ".1.3.6.1.2.1.2.2.1.2"
              is_tag = true
            [[inputs.snmp.table.field]]
              name = "mac-address"
              oid = ".1.3.6.1.2.1.2.2.1.6"
              is_tag = true

            [[inputs.snmp.table.field]]
              name = "actual-mtu"
              oid = ".1.3.6.1.2.1.2.2.1.4"
            [[inputs.snmp.table.field]]
              name = "admin-status"
              oid = ".1.3.6.1.2.1.2.2.1.7"
            [[inputs.snmp.table.field]]
              name = "oper-status"
              oid = ".1.3.6.1.2.1.2.2.1.8"
            [[inputs.snmp.table.field]]
              name = "bytes-in"
              oid = ".1.3.6.1.2.1.31.1.1.1.6"
            [[inputs.snmp.table.field]]
              name = "packets-in"
              oid = ".1.3.6.1.2.1.31.1.1.1.7"
            [[inputs.snmp.table.field]]
              name = "discards-in"
              oid = ".1.3.6.1.2.1.2.2.1.13"
            [[inputs.snmp.table.field]]
              name = "errors-in"
              oid = ".1.3.6.1.2.1.2.2.1.14"
            [[inputs.snmp.table.field]]
              name = "bytes-out"
              oid = ".1.3.6.1.2.1.31.1.1.1.10"
            [[inputs.snmp.table.field]]
              name = "packets-out"
              oid = ".1.3.6.1.2.1.31.1.1.1.11"
            [[inputs.snmp.table.field]]
              name = "discards-out"
              oid = ".1.3.6.1.2.1.2.2.1.19"
            [[inputs.snmp.table.field]]
              name= "errors-out"
              oid= ".1.3.6.1.2.1.2.2.1.20"

          # Wireless interfaces
          [[inputs.snmp.table]]
            name = "snmp-wireless-interfaces"
            inherit_tags = ["hostname"]
            [[inputs.snmp.table.field]]
              name = "ssid"
              oid = ".1.3.6.1.4.1.14988.1.1.1.3.1.4"
              is_tag = true
            [[inputs.snmp.table.field]]
              name = "bssid"
              oid = ".1.3.6.1.4.1.14988.1.1.1.3.1.5"
              is_tag = true

            [[inputs.snmp.table.field]]
              name = "tx-rate"
              oid = ".1.3.6.1.4.1.14988.1.1.1.3.1.2"
            [[inputs.snmp.table.field]]
              name = "rx-rate"
              oid = ".1.3.6.1.4.1.14988.1.1.1.3.1.3"
            [[inputs.snmp.table.field]]
              name = "client-count"
              oid = ".1.3.6.1.4.1.14988.1.1.1.3.1.6"
            [[inputs.snmp.table.field]]
              name = "frequency"
              oid = ".1.3.6.1.4.1.14988.1.1.1.3.1.7"
            [[inputs.snmp.table.field]]
              name = "band"
              oid = ".1.3.6.1.4.1.14988.1.1.1.3.1.8"
            [[inputs.snmp.table.field]]
              name = "noise-floor"
              oid = ".1.3.6.1.4.1.14988.1.1.1.3.1.9"
            [[inputs.snmp.table.field]]
              name = "overall-ccq"
              oid = ".1.3.6.1.4.1.14988.1.1.1.3.1.10"
          # Memory usage (storage/RAM)
          [[inputs.snmp.table]]
            name = "snmp-memory-usage"
            inherit_tags = ["hostname"]
            [[inputs.snmp.table.field]]
              name = "memory-name"
              oid = ".1.3.6.1.2.1.25.2.3.1.3"
              is_tag = true
            [[inputs.snmp.table.field]]
              name = "total-memory"
              oid = ".1.3.6.1.2.1.25.2.3.1.5"
            [[inputs.snmp.table.field]]
              name = "used-memory"
              oid = ".1.3.6.1.2.1.25.2.3.1.6"
      '';
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
    script = builtins.concatStringsSep "\n" (lib.mapAttrsToList (key: value:
      "curl -sSL ${value} | sed 's|^0.0.0.0 ||g' | sed 's|^127.0.0.1 ||g' | sed 's|^\\s*#.*||g' | sed 's|^\\s*::.*||g' | sed 's|^|0.0.0.0 |g' >> ${dnsmasq_filters_path}.tmp")
      dnsmasq_filters) + ''

        cat ${dnsmasq_filters_path}.tmp | sort -u > ${dnsmasq_filters_path}
        rm -rf ${dnsmasq_filters_path}.tmp
      '';
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
