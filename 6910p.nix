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

  beats_port = 9515;
  dnsmasq_port = 53;
  syslog_port = 9514;

  ip_ap = "10.0.0.3";
  ip_imac_wifi = "10.0.10.1";
  ip_imac_eth = "10.0.10.2";
  ip_nas = "10.0.0.32";
  ip_pine = "10.0.10.10";
  ip_raspi4 = "10.0.10.38";
  ip_ux305c = "10.0.10.3";
in {
  imports = [ ./6910p-hardware.nix ./6910p-secrets.nix ./modules/packetbeat.nix ];

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
    firewall = {
      allowPing = false;
      allowedTCPPorts = [];
      allowedUDPPorts = [];
      enable = true;
      extraCommands = ''
        iptables -A INPUT -p tcp -s 10.0.0.0/8 --dport ${toString dnsmasq_port} -m conntrack --ctstate NEW,ESTABLISHED -j ACCEPT
        iptables -A INPUT -p udp -s 10.0.0.0/8 --dport ${toString dnsmasq_port} -m udp -j ACCEPT

        iptables -A INPUT -p tcp -s ${ip_ux305c}/32 --dport ${toString (builtins.head config.services.openssh.ports)} -m conntrack --ctstate NEW,ESTABLISHED -j ACCEPT
        iptables -A INPUT -p tcp -s ${ip_ux305c}/32 --dport ${toString config.services.kibana.port} -m conntrack --ctstate NEW,ESTABLISHED -j ACCEPT

        iptables -A INPUT -p udp -s ${ip_nas}/32 --dport ${toString syslog_port} -m udp -j ACCEPT
        iptables -A INPUT -p udp -s ${ip_pine}/32 --dport ${toString syslog_port} -m udp -j ACCEPT
        iptables -A INPUT -p udp -s ${ip_raspi4}/32 --dport ${toString syslog_port} -m udp -j ACCEPT
        iptables -A INPUT -p udp -s ${ip_ux305c}/32 --dport ${toString syslog_port} -m udp -j ACCEPT

        iptables -A INPUT -p tcp -s ${ip_nas}/32 --dport ${toString syslog_port} -m conntrack --ctstate NEW,ESTABLISHED -j ACCEPT
        iptables -A INPUT -p tcp -s ${ip_pine}/32 --dport ${toString syslog_port} -m conntrack --ctstate NEW,ESTABLISHED -j ACCEPT
        iptables -A INPUT -p tcp -s ${ip_raspi4}/32 --dport ${toString syslog_port} -m conntrack --ctstate NEW,ESTABLISHED -j ACCEPT
        iptables -A INPUT -p tcp -s ${ip_ux305c}/32 --dport ${toString syslog_port} -m conntrack --ctstate NEW,ESTABLISHED -j ACCEPT

        iptables -A INPUT -p tcp -s ${ip_pine}/32 --dport ${toString beats_port} -m conntrack --ctstate NEW,ESTABLISHED -j ACCEPT
        iptables -A INPUT -p tcp -s ${ip_raspi4}/32 --dport ${toString beats_port} -m conntrack --ctstate NEW,ESTABLISHED -j ACCEPT
        iptables -A INPUT -p tcp -s ${ip_ux305c}/32 --dport ${toString beats_port} -m conntrack --ctstate NEW,ESTABLISHED -j ACCEPT
      '';
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

    elasticsearch = {
      enable = true;
      extraConf = ''
        cluster.max_shards_per_node: 100000
        node.name: "elasticsearch"
        node.master: true
        node.data: true
      '';
      package = pkgs.elasticsearch7-oss;
    };

    elasticsearch-curator = {
      actionYAML = ''
        ---
        actions:
          1:
            action: delete_indices
            options:
              allow_ilm_indices: true
              ignore_empty_list: True
              disable_action: False
            filters:
            - filtertype: pattern
              kind: prefix
              value: nequi-
            - filtertype: age
              source: name
              direction: older
              timestring: '%Y.%m.%d'
              unit: days
              unit_count: 180
      '';
      enable = true;
    };

    journalbeat = {
      enable = true;
      extraConfig = ''
        logging.metrics.enabled: false

        journalbeat.inputs:
          - paths: []

        output.elasticsearch:
          hosts: ["http://localhost:${toString config.services.elasticsearch.port}"]
      '';
      package = pkgs.journalbeat7;
    };

    kibana = {
      elasticsearch.hosts = [ "http://localhost:${toString config.services.elasticsearch.port}" ];
      enable = true;
      listenAddress = "0.0.0.0";
      package = pkgs.kibana7-oss;
    };

    locate.enable = true;

    logind.extraConfig = "HandleLidSwitch=ignore";

    logstash = {
      enable = true;

      inputConfig = ''
        syslog {
          port => ${toString syslog_port}
          type => syslog
        }
        beats {
          port => ${toString beats_port}
        }
      '';

      filterConfig = ''
        if [@metadata][beat] {
          mutate { remove_field => [ "type" ] }
          mutate { add_field => { "type" => "%{[@metadata][beat]}" } }
        }
      '';

      outputConfig = ''
        elasticsearch {
          hosts => ["localhost:${toString config.services.elasticsearch.port}"]
          index => "%{type}-%{+YYYY.MM.dd}"
        }
      '';

      package = pkgs.logstash7-oss;
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

    openssh = {
      enable = true;
      passwordAuthentication = false;
      permitRootLogin = "no";
    };

    packetbeat = {
      enable = true;
      package = pkgs.packetbeat7;

      protocols = {
        dhcpv4.enable = true;
        dns.enable = true;
      };
#
#      extraConfig = ''
#        packetbeat.flows:
#          timeout: 30s
#          period: 10s
#
#        packetbeat.protocols:
#        - type: icmp
#          enabled: true
#        - type: dhcpv4
#          ports: [67, 68]
#        - type: dns
#          ports: [53]
#        - type: http
#          ports: [80, 8080, 8000, 5000, 8002]
#        - type: amqp
#          ports: [5672]
#        - type: cassandra
#          ports: [9042]
#        - type: memcache
#          ports: [11211]
#        - type: mysql
#          ports: [3306,3307]
#        - type: redis
#          ports: [6379]
#        - type: pgsql
#          ports: [5432]
#        - type: thrift
#          ports: [9090]
#        - type: tls
#          ports: [443, 993, 995, 5223, 8443, 8883, 9243]
      #
      extraConfig = ''
        output.elasticsearch:
          hosts: ["http://localhost:${toString config.services.elasticsearch.port}"]
      '';
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
    script = builtins.concatStringsSep "\n" (lib.mapAttrsToList (key: value:
      "curl -sSL ${value} | sed 's|^0.0.0.0 ||g' | sed 's|^127.0.0.1 ||g' | sed 's|^\\s*#.*||g' | sed 's|^\\s*::.*||g' | sed 's|^|0.0.0.0 |g' >> ${dnsmasq_filters_path}.tmp")
      dnsmasq_filters) + ''

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
