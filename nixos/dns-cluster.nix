{
  lib,
  ipv4Address,
  ...
}:
let
  cluster_members = [
    {
      ip = "10.0.0.54";
      host = "opi5plus.nequissimus.com";
    }
    {
      ip = "10.0.0.53";
      host = "rpi4b.nequissimus.com";
    }
  ];
  primary_ip = "10.0.0.54";
  is_primary = (ipv4Address == primary_ip);

  blocky_prometheus_port = 4000;

  postgres_database = "blocky";
  postgres_port = 5432;
  postgres_prometheus_port = 9187;
  postgres_user = "blocky";

  redis_prometheus_port = 9121;

  allowed_domains = [
    "*.amazonaws.com" # AWS
    "api-us.roborock.com" # Roborock
    "awsusor0.fds.api.xiaomi.com" # Roborock
    "*.edgesuite.net" # Akamai CDN
    "ecsv2.roblox.com" # Roblox tracking
    "go.aftvnews.com" # AFTVNews Downloader
    "huggingface.co" # ML Models
    "mqtt-mini.facebook.com" # Facebook messenger
    "mqtt-us.roborock.com" # Roborock
    "*.olg.ca" # OLG
    "*.tile-api.com" # Tile API
    "*.torproject.org" # TOR
    "tr.rbxcdn.com" # Roblox assets
    "track.spe.schoolmessenger.com" # School info
    "transport.home.nest.com" # Nest status updates
    "web.poecdn.com" # Path Of Exile website
    "x20na.update.easebar.com" # Marvel Rivals
  ];

  blocked_domains = [
    "api-fp-retry-bj.fengkongcloud.com" # Fingerprinting
  ];

  blocklists_mixed = [
    "https://big.oisd.nl/domainswild2"
    "https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews-gambling-porn/hosts"
    "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"
  ]
  ++ (lib.concatMap
    (file: [
      "https://cdn.jsdelivr.net/gh/hagezi/dns-blocklists@latest/wildcard/${file}"
      "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/${file}"
      "https://gitlab.com/hagezi/mirror/-/raw/main/dns-blocklists/wildcard/${file}"
    ])
    [
      "anti.piracy.txt"
      "doh-vpn-proxy-bypass.txt"
      "doh.txt"
      "dyndns.txt"
      "gambling.txt"
      "hoster.txt"
      "native.amazon.txt"
      "native.apple.txt"
      "native.huawei.txt"
      "native.lgwebos.txt"
      "native.oppo-realme.txt"
      "native.roku.txt"
      "native.samsung.txt"
      "native.tiktok.txt"
      "native.vivo.txt"
      "native.winoffice.txt"
      "native.xiaomi.txt"
      "nosafesearch.txt"
      "nsfw.txt"
      "pro.plus.txt"
      "spam-tlds-onlydomains.txt"
      "tif.txt"
      "urlshortener.txt"
    ]
  );

  blocklists_newdomains = [
    "https://raw.githubusercontent.com/hagezi/nrd/main/domains/dga14.txt"
    "https://raw.githubusercontent.com/hagezi/nrd/main/domains/dga30.txt"
    "https://raw.githubusercontent.com/hagezi/nrd/main/domains/dga7.txt"
    "https://raw.githubusercontent.com/hagezi/nrd/main/domains/nrd14-8.txt"
    "https://raw.githubusercontent.com/hagezi/nrd/main/domains/nrd21-15.txt"
    "https://raw.githubusercontent.com/hagezi/nrd/main/domains/nrd28-22.txt"
    "https://raw.githubusercontent.com/hagezi/nrd/main/domains/nrd35-29.txt"
    "https://raw.githubusercontent.com/hagezi/nrd/main/domains/nrd7.txt"
  ];
in
{
  environment.etc."dns-allowlist.txt".text = builtins.concatStringsSep "\n" allowed_domains;
  environment.etc."dns-blocklist.txt".text = builtins.concatStringsSep "\n" blocked_domains;

  networking.firewall = {
    allowedTCPPorts = [
      53
    ]
    ++ lib.optionals (!is_primary) [
      blocky_prometheus_port
      postgres_prometheus_port
      redis_prometheus_port
    ]
    ++ lib.optionals is_primary [ postgres_port ];
    allowedUDPPorts = [ 53 ];
  };

  services = {
    blocky = {
      enable = true;
      settings = {
        blocking = {
          allowlists.all = [
            "/etc/dns-allowlist.txt"
            "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/spam-tlds-allow-onlydomains.txt"
          ];

          blockTTL = "1h";

          clientGroupsBlock = {
            default = [
              "all"
              "manual"
              "newdomains"
            ];
          };

          denylists = {
            all = blocklists_mixed;
            manual = [ "/etc/dns-blocklist.txt" ];
            newdomains = blocklists_newdomains;
          };

          loading = {
            concurrency = 4;

            downloads = {
              readTimeout = "600s";
              timeout = "600s";
              writeTimeout = "600s";
            };

            maxErrorsPerSource = 10;

            refreshPeriod = "4h";
          };
        };

        caching = {
          maxItemsCount = 10000;
          minTime = "1h";
        };

        clientLookup.upstream = "10.0.0.2";

        customDNS = {
          mapping = builtins.listToAttrs (
            map (x: {
              name = x.host;
              value = x.ip;
            }) cluster_members
          );

          zone = ''
            ''\$ORIGIN time.google.com.
            @ 3600 CNAME ca.pool.ntp.org.

            ''$ORIGIN ntp.aliyun.com.
            @ 3600 CNAME ca.pool.ntp.org.
          '';
        };

        ports = {
          http = "0.0.0.0:${builtins.toString blocky_prometheus_port}";
        };

        prometheus = {
          enable = true;
          path = "/metrics";
        };

        queryLog = {
          logRetentionDays = 14;
          target = "postgres://${postgres_user}:blocky123@${primary_ip}:${builtins.toString postgres_port}/${postgres_database}";
          type = "postgresql";
        };

        upstreams = {
          groups.default = [
            "https://dns.adguard-dns.com/dns-query"
          ];

          strategy = "strict";
        };
      };
    };

    postgresql = {
      authentication = lib.mkOverride 10 (
        ''
          #type database  DBuser  auth-method
          local all  all  trust
        ''
        + (lib.strings.concatLines (
          lib.map (member: "host ${postgres_database}  all  ${member.ip}/32 scram-sha-256") cluster_members
        ))
      );

      enable = is_primary;
      enableTCPIP = true;
      ensureDatabases = [ postgres_database ];

      ensureUsers = [
        {
          ensureClauses = {
            login = true;
            password = "SCRAM-SHA-256$4096:AKFW5HQyvIdGrSBpzOWMCA==$Wfj+0wTrvnoWy0IvAmj1t8YWHGvaBKA4xO6mZdPiXzw=:W6oNDbfmFTjkzk9s8af31FoKhKNDUhghlR/H7RBwKQc=";
          };

          ensureDBOwnership = true;
          name = postgres_user;
        }
      ];

      settings.port = postgres_port;
    };

    prometheus.exporters = {
      # postgres = {
      #   enable = is_primary;
      #   listenAddress = "0.0.0.0";
      #   port = postgres_prometheus_port;
      # };

      redis = {
        enable = is_primary;
        port = redis_prometheus_port;
      };
    };

    redis.servers.blocky = {
      bind = "0.0.0.0";
      databases = 1;
      enable = is_primary;
      openFirewall = is_primary;
      port = 6379;
      syslog = false;
    };
  };
}
