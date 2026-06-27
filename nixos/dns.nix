{
  config,
  lib,
  pkgs,
  ...
}:

let
  allowed_domains = [
    "amazonaws.com" # AWS
    "api-us.roborock.com" # Roborock
    "awsusor0.fds.api.xiaomi.com" # Roborock
    "edgesuite.net" # Akamai CDN
    "ecsv2.roblox.com" # Roblox tracking
    "huggingface.co" # ML Models
    "mqtt-mini.facebook.com" # Facebook messenger
    "mqtt-us.roborock.com" # Roborock
    "olg.ca" # OLG
    "tile-api.com" # Tile API
    "torproject.org" # TOR
    "tr.rbxcdn.com" # Roblox assets
    "track.spe.schoolmessenger.com" # School info
    "transport.home.nest.com" # Nest status updates
    "web.poecdn.com" # Path Of Exile website
    "x20na.update.easebar.com" # Marvel Rivals
  ];

  blocked_domains = [
    "api-fp-retry-bj.fengkongcloud.com" # Fingerprinting
    "ntp.aliyun.com" # Alibaba NTP
    "time.google.com" # Google NTP
  ];

  blocklists_mixed = [
    "https://big.oisd.nl/domainswild2"
    "https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews-gambling-porn/hosts"
    "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/pro.plus-onlydomains.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/ultimate.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/pro.plus.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/pro.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/multi.txt"
  ];

  blocklists_piracy = [
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/anti.piracy.txt"
  ];

  blocklists_gambling = [
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/gambling.medium.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/gambling.txt"
  ];

  blocklists_nsfw = [
    "https://nsfw.oisd.nl/domainswild2"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/nsfw.txt"
  ];

  blocklists_newdomains = [
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/domains/dga14.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/domains/dga30.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/domains/dga7.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/domains/nrd7.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/domains/nrd14-8.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/domains/nrd21-15.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/domains/nrd28-22.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/domains/nrd35-29.txt"
  ];

  blocklists_vendors = [
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/native.samsung.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/native.winoffice.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/native.xiaomi.txt"
  ];

  blocklists_misc = [
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/doh.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/tif.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/hoster-onlydomains.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/spam-tlds-onlydomains.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/doh-vpn-proxy-bypass.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/dyndns.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/hoster.txt"
  ];

  custom_blocky = pkgs.buildGoModule (finalAttrs: {
    pname = "blocky";
    version = "0.32.0";

    src = pkgs.fetchFromGitHub {
      owner = "0xERR0R";
      repo = "blocky";
      rev = "v${finalAttrs.version}";
      hash = "sha256-qgozdPDa6lpEbt1lT97E3UC09TX+/khsKirm8AMAwDk=";
    };

    doCheck = false;

    vendorHash = "sha256-OsExQUgqAdiPp6GI7ebNE0XBvwQrFtkEnbnPu3268bc=";

    ldflags = [
      "-s"
      "-w"
      "-X github.com/0xERR0R/blocky/util.Version=${finalAttrs.version}"
    ];

    meta.mainProgram = "blocky";
  });

in
{
  environment.etc."dns-allowlist.txt".text = builtins.concatStringsSep "\n" allowed_domains;
  environment.etc."dns-blocklist.txt".text = builtins.concatStringsSep "\n" blocked_domains;

  networking.firewall = {
    enable = lib.mkDefault true;

    allowedTCPPorts = [ 53 ];
    allowedUDPPorts = [ 53 ];
  };

  services.blocky = {
    enable = true;

    package = custom_blocky;

    settings = {
      blocking = {
        allowlists.misc = [
          "/etc/dns-allowlist.txt"
          "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/spam-tlds-allow-onlydomains.txt"
        ];

        blockTTL = "1h";

        clientGroupsBlock = {
          default = [
            "gambling"
            "manual"
            "misc"
            "mixed"
            "newdomains"
            "nsfw"
            "piracy"
            "vendors"
          ];
        };

        denylists = {
          gambling = blocklists_gambling;
          manual = [ "/etc/dns-blocklist.txt" ];
          misc = blocklists_misc;
          mixed = blocklists_mixed;
          newdomains = blocklists_newdomains;
          nsfw = blocklists_nsfw;
          piracy = blocklists_piracy;
          vendors = blocklists_vendors;
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

      customDNS = {
        mapping = {
          "opi5plus.nequissimus.com" = "10.0.0.54";
        };
      };

      ports = {
        http = "127.0.0.1:4000";
      };

      prometheus = {
        enable = true;
        path = "/metrics";
      };

      queryLog = {
        logRetentionDays = 14;
        target = "/var/lib/blocky/querylog.db";
        type = "sqlite";
      };

      upstreams = {
        groups.default = [
          "https://dns.adguard-dns.com/dns-query"
        ];

        strategy = "strict";
      };
    };
  };

  systemd.tmpfiles.rules = [ "d /var/lib/private 0701 nobody nogroup" ];
}
