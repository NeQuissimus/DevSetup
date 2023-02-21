{ config, pkgs, lib, ... }:
let
  # TODO
  prometheus_mikrotik_user = "foo";
  prometheus_mikrotik_password = "bar";

  dnsmasq_filters_path = "/etc/dnsmasq/filters";

  dnsmasq_filters = {
    abuse_ch = "https://urlhaus.abuse.ch/downloads/hostfile/";
    disconnectme_ad =
      "https://s3.amazonaws.com/lists.disconnect.me/simple_ad.txt";
    disconnectme_tracking =
      "https://s3.amazonaws.com/lists.disconnect.me/simple_tracking.txt";
    notracking =
      "https://raw.githubusercontent.com/notracking/hosts-blocklists/master/hostnames.txt";
    sinfonietta_gambling =
      "https://raw.githubusercontent.com/Sinfonietta/hostfiles/master/gambling-hosts";
    sinfonietta_porn =
      "https://raw.githubusercontent.com/Sinfonietta/hostfiles/master/pornography-hosts";
    sinfonietta_snuff =
      "https://raw.githubusercontent.com/Sinfonietta/hostfiles/master/snuff-hosts";
    steven_black =
      "https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews-gambling-porn/hosts";
    ultimate_hosts =
      "https://raw.githubusercontent.com/Ultimate-Hosts-Blacklist/Ultimate.Hosts.Blacklist/master/hosts/hosts0";
  };

  dhcp = ipAddress: hostName: ethernetAddress: {
    ipAddress = ipAddress;
    hostName = hostName;
    ethernetAddress = ethernetAddress;
  };

  dnsmasq_port = 53;

  ip_router = "10.0.0.2";
  ip_switch = "10.0.0.4";
  ip_u6pro = "10.0.0.5";
  ip_u6ext = "10.0.0.6";
  ip_c220 = "10.0.0.52";

  pkgs_promExporter = (import ./nixpkgs/prometheus-exporter-dhcpd.nix { inherit pkgs; });
in {
  imports = [
    ./c220-hardware.nix

    ./nixos/kernel.nix
    ./nixos/nix.nix
    ./nixos/security.nix
    ./nixos/ssh.nix
    ./nixos/users.nix
  ];

  console.keyMap = "us";

  documentation.nixos.enable = false;

  environment = {
    etc."prometheus.sh" = {
      text = ''
        cp /var/lib/dnsmasq/dnsmasq.log /var/lib/dnsmasq/tmp.log
        echo "" > /var/lib/dnsmasq/dnsmasq.log
        grep '/etc/dnsmasq/filters' /var/lib/dnsmasq/tmp.log | grep -v ' read /etc/dnsmasq/filters' | sed 's|.*/etc/dnsmasq/filters \(.*\) is \(.*\)|dnsmasq_resolution{domain="\1",ip="\2"}|g' | sort | uniq -c | awk '{print $2 " " $1}' > /var/lib/prometheus/blocked.prom
        grep 'reply' /var/lib/dnsmasq/tmp.log | grep -v 'query is duplicate' | grep -v '<CNAME>' | sed 's|.* reply \(.*\) is \(.*\)|dnsmasq_resolution{domain="\1",ip="\2"}|g' | sort | uniq -c | awk '{print $2 " " $1}' > /var/lib/prometheus/resolved.prom
        grep 'query' /var/lib/dnsmasq/tmp.log | grep -v 'query is duplicate' | sed 's|.*query\[\(.*\)\] \(.*\) from \(.*\)|dnsmasq_queries{type="\1",domain="\2",origin="\3"}|g' | sed 's|type="type=65"|type="HTTPS"|g' | sed 's|type="type=64"|type="SVCB"|g' | sort | uniq -c | awk '{print $2 " " $1}' > /var/lib/prometheus/queries.prom
        rm -f /var/lib/dnsmasq/tmp.log
        cat /var/lib/dhcpd4/dhcpd.leases | sed -E 's|((starts\|ends\|cltt\|tstp).*[0-9]);|\1 EST;|g' > /tmp/leases
        timeout -k 10 30s ${pkgs_promExporter}/dhcpd_lease_exporter.py -l /tmp/leases -t /var/lib/prometheus/leases.prom
      '';
      mode = "0777";
    };

    systemPackages = with pkgs; [];
  };

  i18n = { defaultLocale = "en_US.UTF-8"; };

  networking = {
    defaultGateway = "10.0.0.2";

    firewall = {
      allowPing = false;
      allowedTCPPorts = [ 80 8443 ];
      allowedUDPPorts = [ ];
      enable = true;
      extraCommands = ''
        iptables -A INPUT -p tcp -s 10.0.0.0/8 --dport ${
          toString dnsmasq_port
        } -m conntrack --ctstate NEW,ESTABLISHED -j ACCEPT
        iptables -A INPUT -p udp -s 10.0.0.0/8 --dport ${
          toString dnsmasq_port
        } -m udp -j ACCEPT

        iptables -A INPUT -p udp -s ${ip_u6pro}/32 --dport 3478 -m udp -j ACCEPT
        iptables -A INPUT -p udp -s ${ip_u6pro}/32 --dport 10001 -m udp -j ACCEPT
        iptables -A INPUT -p tcp -s ${ip_u6pro}/32 --dport 8443 -m conntrack --ctstate NEW,ESTABLISHED -j ACCEPT
        iptables -A INPUT -p tcp -s ${ip_u6pro}/32 --dport 8080 -m conntrack --ctstate NEW,ESTABLISHED -j ACCEPT
        iptables -A INPUT -p udp -s ${ip_u6ext}/32 --dport 3478 -m udp -j ACCEPT
        iptables -A INPUT -p udp -s ${ip_u6ext}/32 --dport 10001 -m udp -j ACCEPT
        iptables -A INPUT -p tcp -s ${ip_u6ext}/32 --dport 8443 -m conntrack --ctstate NEW,ESTABLISHED -j ACCEPT
        iptables -A INPUT -p tcp -s ${ip_u6ext}/32 --dport 8080 -m conntrack --ctstate NEW,ESTABLISHED -j ACCEPT
      '';
    };

    hostId = "8990a246";
    hostName = "cyclops";

    interfaces."enp4s0f0" = {
      ipv4.addresses = [{
        address = ip_c220;
        prefixLength = 16;
      }];
      ipv6.addresses = [{
        address = "fd00:1873::100";
        prefixLength = 117;
      }];
    };

    nameservers = [ "9.9.9.9" ];
    useDHCP = false;
    usePredictableInterfaceNames = true;
  };

  services = {
    cron = {
      enable = true;
      systemCronJobs = [ 
        "0 17 * * * root reboot" 
        "*/5 * * * * root /etc/prometheus.sh"
      ];
    };

    dhcpd4 = {
      enable = true;

      extraConfig = ''
        option subnet-mask 255.255.0.0;
        option broadcast-address 10.0.255.255;
        option routers 10.0.0.2;
        option domain-name-servers ${ip_c220};
        option domain-name "nequissimus.com";
        subnet 10.0.0.0 netmask 255.255.0.0 {
          range 10.0.255.1 10.0.255.100;
          default-lease-time 600;
          max-lease-time 3600;
        }
      '';

      interfaces = [ ]; # All interfaces

      machines = [
        (dhcp ip_switch "mikrotik-switch-ether" "DC:2C:6E:2D:F5:BF")
        (dhcp "10.0.0.5" "unifi-u6-pro" "60:22:32:1C:FA:C9")
        (dhcp "10.0.0.6" "unifi-u6-extender" "D0:21:F9:F2:D2:BC")
        (dhcp "10.0.0.33" "buffalo-nas-ether" "DC:FB:02:DA:11:C1")
        (dhcp "10.0.0.37" "supermicro-ether" "0C:C4:7A:09:d7:F9")
      ];
    };

    dnscrypt-proxy2 = {
      enable = true;

      settings = {
        cache = true;
        cache_max_ttl = 600;
        cache_size = 1024;
        cert_refresh_delay = 240;
        doh_servers = true;
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
        domain-needed
        bogus-priv
        addn-hosts=${dnsmasq_filters_path}
        all-servers
        cache-size=20000
        local-ttl=3600
        min-cache-ttl=600
        log-queries
        log-facility=/var/lib/dnsmasq/dnsmasq.log
        port=${toString dnsmasq_port}
      '';
      servers = [ "127.0.0.1#5300" ];
    };

    grafana = {
      enable = true;
      settings.server = {
        domain = "grafana.nequissimus.com";
        http_addr = "127.0.0.1";
        http_port = 3000;
      };
    };

    nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;

      virtualHosts.${config.services.grafana.settings.server.domain} = {
        locations."/" = {
          proxyPass =
            "http://127.0.0.1:${toString config.services.grafana.settings.server.http_port}";
          proxyWebsockets = true;
        };
      };
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

    openssh.enable = true;

    prometheus = {
      enable = true;

      exporters = {
        dnsmasq = {
          dnsmasqListenAddress = "127.0.0.1:${toString dnsmasq_port}";
          enable = true;
          leasesPath = "/var/lib/dnsmasq/dnsmasq.leases";
        };

        mikrotik = {
          configuration = {
            devices = [{
              name = "router";
              address = "${ip_router}";
              user = "${prometheus_mikrotik_user}";
              password = "${prometheus_mikrotik_password}";
            }];
            features = {
              bgp = true;
              dhcp = true;
              routes = true;
              optics = true;
            };
          };
          enable = true;
        };

        node = {
          enable = true;
          enabledCollectors =
            [ "cpu" "filesystem" "meminfo" "netdev" "os" "systemd" "textfile" ];
          extraFlags = [ "--collector.textfile.directory=/var/lib/prometheus" ];
        };

        unifi-poller = {
          controllers = [{
            url = "https://127.0.0.1:8443";
          }];

          enable = true;
          listenAddress = "127.0.0.1";
        };

        zfs = { enable = true; };
      };

      port = 9001;

      retentionTime = "30d";

      scrapeConfigs = [
        {
          job_name = "dnsmasq";
          static_configs = [{
            targets = [
              "127.0.0.1:${
                toString config.services.prometheus.exporters.dnsmasq.port
              }"
            ];
          }];
        }
        {
          job_name = "mikrotik";
          static_configs = [{
            targets = [
              "127.0.0.1:${
                toString config.services.prometheus.exporters.mikrotik.port
              }"
            ];
          }];
        }
        {
          job_name = "nodes";
          static_configs = [{
            targets = [
              "127.0.0.1:${
                toString config.services.prometheus.exporters.node.port
              }"
            ];
          }];
        }
        {
          job_name = "unifi";
          static_configs = [{
            targets = [
              "127.0.0.1:${
                toString config.services.prometheus.exporters.unifi-poller.port
              }"
            ];
          }];
        }
        {
          job_name = "zfs";
          static_configs = [{
            targets = [
              "127.0.0.1:${
                toString config.services.prometheus.exporters.zfs.port
              }"
            ];
          }];
        }
      ];
    };

    radvd = {
      config = ''
        interface enp0s25 {
          AdvSendAdvert on;
          prefix fd00:1872:0:1::/64 {
            AdvOnLink on;
            AdvAutonomous on;
          };
        };
      '';

      enable = true;
    };

    unifi = {
      enable = true;
      initialJavaHeapSize = 512;
      maximumJavaHeapSize = 2048;
      openFirewall = true;

      jrePackage = pkgs.openjdk11;
      unifiPackage = (import ./nixpkgs/unifi.nix { inherit pkgs; });
    };

    upower.enable = true;

    zfs.autoScrub = {
      enable = true;
      interval = "weekly";
    };
  };

  systemd = {
    services.dnsblocklists = {
      enable = true;
      description = "Download updated blacklists";
      before = [ "dnsmasq.service" ];
      startAt = "hourly";

      path = with pkgs; [ curl ];
      script = builtins.concatStringsSep "\n" (lib.mapAttrsToList (key: value:
        "curl -sSL ${value} | sed 's|^0.0.0.0 ||g' | sed 's|^127.0.0.1 ||g' | sed 's|^\\s*#.*||g' | sed 's|^\\s*::.*||g' | sed 's|.*shopify.*||g' | sed 's|^|0.0.0.0 |g' >> ${dnsmasq_filters_path}.tmp")
      dnsmasq_filters) + ''
          echo '${ip_router} router.nequissimus.com' >> ${dnsmasq_filters_path}.tmp
          echo '${ip_switch} switch.nequissimus.com' >> ${dnsmasq_filters_path}.tmp
          echo '${ip_c220} cyclops.nequissimus.com' >> ${dnsmasq_filters_path}.tmp
          echo '${ip_c220} ${config.services.grafana.settings.server.domain}' >> ${dnsmasq_filters_path}.tmp

          cat ${dnsmasq_filters_path}.tmp | sort -u > ${dnsmasq_filters_path}
          rm -rf ${dnsmasq_filters_path}.tmp
        '';
    };

    tmpfiles.rules = [
      "d /var/lib/prometheus 0777 prometheus prometheus"
    ];
  };

  time = { timeZone = "America/Toronto"; };
}
