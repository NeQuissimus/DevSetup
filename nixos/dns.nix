{ config, lib, pkgs, ... }:

let
  allowed_domains = [
    "api-us.roborock.com" # Roborock
    "awsusor0.fds.api.xiaomi.com" # Roborock
    "edgesuite.net" # Akamai CDN
    "ecsv2.roblox.com" # Roblox tracking
    "huggingface.co" # ML Models
    "mqtt-mini.facebook.com" # Facebook messenger
    "mqtt-us.roborock.com" # Roborock
    "olg.ca" # OLG
    "tile-api.com" # Tile API
    "tr.rbxcdn.com" # Roblox assets
    "track.spe.schoolmessenger.com" # School info
    "transport.home.nest.com" # Nest status updates
    "web.poecdn.com" # Path Of Exile website
    "x20na.update.easebar.com" # Marvel Rivals
  ];

  blocked_domains = [
    "api-fp-retry-bj.fengkongcloud.com" # Fingerprinting
    "ntp.aliyun.com" # Alibaba NTP
  ];

  blocklists = [
    "https://blocklistproject.github.io/Lists/abuse.txt"
    "https://blocklistproject.github.io/Lists/ads.txt"
    "https://blocklistproject.github.io/Lists/fraud.txt"
    "https://blocklistproject.github.io/Lists/gambling.txt"
    "https://blocklistproject.github.io/Lists/malware.txt"
    "https://blocklistproject.github.io/Lists/phishing.txt"
    "https://blocklistproject.github.io/Lists/porn.txt"
    "https://blocklistproject.github.io/Lists/scam.txt"
    "https://blocklistproject.github.io/Lists/smart-tv.txt"
    "https://blocklistproject.github.io/Lists/tracking.txt"
    "https://perflyst.github.io/PiHoleBlocklist/AmazonFireTV.txt"
    "https://perflyst.github.io/PiHoleBlocklist/android-tracking.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/gambling.medium.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/hosts/doh.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/hosts/tif.txt"
    "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/pro.plus-onlydomains.txt"
    "https://raw.githubusercontent.com/laylavish/uBlockOrigin-HUGE-AI-Blocklist/main/noai_hosts.txt"
    "https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews-gambling-porn/hosts"
    "https://raw.githubusercontent.com/xRuffKez/NRD/refs/heads/main/lists/30-day/domains-only/nrd-30day_part1.txt"
    "https://raw.githubusercontent.com/xRuffKez/NRD/refs/heads/main/lists/30-day/domains-only/nrd-30day_part2.txt"
    "https://someonewhocares.org/hosts/zero/hosts"
  ];

  dockerImages.technitium =
    "technitium/dns-server:13.6.0@sha256:b12182649647f7e28dd596f4089032099a94ab1bee2262d6679fa185238c5f02";
in {
  networking.firewall = {
    enable = lib.mkDefault true;

    allowedTCPPorts = [ 53 ];
    allowedUDPPorts = [ 53 ];
  };

  virtualisation.oci-containers = {
    backend = "docker";

    containers = {
      technitium = {
        autoStart = true;

        environment = {
          DNS_SERVER_DOMAIN = "technitium";
          DNS_SERVER_ADMIN_PASSWORD = "admin";
          DNS_SERVER_WEB_SERVICE_LOCAL_ADDRESSES = "0.0.0.0";
          DNS_SERVER_WEB_SERVICE_ENABLE_HTTPS = "false";
          DNS_SERVER_ENABLE_BLOCKING = "true";
          DNS_SERVER_BLOCK_LIST_URLS = lib.concatStringsSep "," blocklists;
          DNS_SERVER_FORWARDERS = "9.9.9.9,149.112.112.112";
          DNS_SERVER_LOG_USING_LOCAL_TIME = "true";
        };

        extraOptions = [
          "--log-driver"
          "syslog"
          "--log-opt"
          "syslog-address=udp://10.0.0.52:5514"
          "--log-opt"
          "syslog-format=rfc5424"
        ];

        image = dockerImages.technitium;
        ports = [ "53:53/tcp" "53:53/udp" "5380:5380/tcp" ];
        volumes = [ "/var/lib/technitium:/etc/dns" ];
      };
    };
  };

  systemd = {
    services.technitium-config = {
      after = [ "docker-technitium.service" ];

      description = "Configure Technitium";

      path = with pkgs; [ curl ];

      script = ''
        export TOKEN=$(</var/lib/technitium/token)
        ${lib.concatMapStringsSep "\n" (x:
          ''
            curl "http://localhost:5380/api/allowed/add?token=$TOKEN&domain=${x}"'')
        allowed_domains}
        ${lib.concatMapStringsSep "\n" (x:
          ''
            curl "http://localhost:5380/api/blocked/add?token=$TOKEN&domain=${x}"'')
        blocked_domains}
      '';
      wantedBy = [ "multi-user.target" ];
    };

    tmpfiles.rules = [ "d /var/lib/technitium 0755 nequi docker" ];
  };
}
