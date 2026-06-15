{ config, lib, pkgs, ... }:

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

  blocklists = [
    "https://adguardteam.github.io/AdGuardSDNSFilter/Filters/adguard_popup_filter.txt"
    "https://adguardteam.github.io/AdGuardSDNSFilter/Filters/filter.txt"
    "https://big.oisd.nl/domainswild2"
    "https://nsfw.oisd.nl/domainswild2"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/anti.piracy.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/dga14.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/dga30.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/dga7.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/doh-vpn-proxy-bypass.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/dyndns.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/gambling.medium.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/gambling.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/hoster.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/multi.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/native.samsung.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/native.winoffice.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/native.xiaomi.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/nrd14-8.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/nrd21-15.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/nrd28-22.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/nrd35-29.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/nrd7.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/nsfw.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/pro.plus.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/pro.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/spam-tlds-adblock.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/spam-tlds.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/tif.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/ultimate.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/whitelist-referral.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/hosts/doh.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/hosts/tif.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/hoster-onlydomains.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/pro.plus-onlydomains.txt"
    "https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews-gambling-porn/hosts"
    "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"
  ];
in {
  environment.etc."dns-allowlist.txt".text = builtins.concatStringsSep "\n" allowed_domains;
  environment.etc."dns-blocklist.txt".text = builtins.concatStringsSep "\n" blocked_domains;

  networking.firewall = {
    enable = lib.mkDefault true;

    allowedTCPPorts = [ 53 ];
    allowedUDPPorts = [ 53 ];
  };

  services.blocky = {
    enable = true;
    enableConfigCheck = false;

    settings = {
      blocking = {
        allowlists.ads = [
          "/etc/dns-allowlist.txt"
        ];

        blockTTL = "1h";

        denylists.ads = blocklists ++ [ "/etc/dns-blocklist.txt" ];
      };

      caching = {
        maxItemsCount = 10000;
        minTime = "1h";
      };

      ports = {
        dns = 53;
        http = "127.0.0.1:4000";
      };

      prometheus = {
        enable = true;
        path = "/metrics";
      };

      upstreams = {
        groups.default = [
          "quic://dns.adguard-dns.com"
          "https://dns.adguard-dns.com/dns-query"
        ];

        strategy = "strict";
      };
    };
  };
}
