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

  dockerImages.technitium =
    "technitium/dns-server:15.2.0@sha256:23d3b63d959e997800b095fe93009b3fae271b5258234ff2ade8535cb33682c8";
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
          DNS_SERVER_FORWARDERS = "https://dns.adguard-dns.com/dns-query";
          DNS_SERVER_FORWARDER_PROTOCOL = "Https";
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
      };
    };
  };

  systemd = {
    services.technitium-config = {
      after = [ "docker-technitium.service" ];

      description = "Configure Technitium";

      path = with pkgs; [ curl jq ];

      script = ''
        while [ "$(curl -o /dev/null -s -w \"%{http_code}\" http://localhost:5380/api)" != "\"404\"" ]; do sleep 1; done

        export TOKEN=$(curl -sSL 'http://localhost:5380/api/user/createToken?user=admin&pass=admin&tokenName=MyToken1' | jq '.token' -r)
        export SQLITE_URL=$(curl -sSL "http://localhost:5380/api/apps/listStoreApps?token=$TOKEN" | jq '.response.storeApps[] | select(.name | contains("Query Logs (Sqlite)")) | .url' -r)

        curl -sSL "http://localhost:5380/api/apps/downloadAndInstall?token=$TOKEN&name=QueryLogs&url=$SQLITE_URL"

        curl -sSL "http://localhost:5380/api/settings/set?token=$TOKEN&blockListUpdateIntervalHours=4&blockListUrls=${
          lib.concatStringsSep "," blocklists
        }"

        curl -sSL "http://localhost:5380/api/settings/forceUpdateBlockLists?token=$TOKEN"

        ${lib.concatMapStringsSep "\n" (x: ''
          curl "http://localhost:5380/api/allowed/add?token=$TOKEN&domain=${x}"
        '') allowed_domains}
        ${lib.concatMapStringsSep "\n" (x: ''
          curl "http://localhost:5380/api/blocked/add?token=$TOKEN&domain=${x}"
        '') blocked_domains}

        curl -sSL "http://localhost:5380/api/zones/delete?token=$TOKEN&zone=google.com"

        curl -sSL "http://localhost:5380/api/zones/create?token=$TOKEN&zone=google.com&type=Forwarder&initializeForwarder=true&forwarder=this-server"

        curl -sSL "http://localhost:5380/api/zones/records/delete?token=$TOKEN&domain=time.google.com&zone=google.com&type=CNAME"

        curl -sSL "http://localhost:5380/api/zones/records/add?token=$TOKEN&domain=time.google.com&zone=google.com&type=CNAME&cname=ca.pool.ntp.org"

        curl -sSL "http://localhost:5380/api/cache/flush?token=$TOKEN"

        curl -sSL "http://localhost:5380/api/user/logout?token=$TOKEN"
      '';
      wantedBy = [ "multi-user.target" ];
    };

    timers.technitium-config = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "hourly";
        Unit = "technitium-config.service";
      };
    };

    tmpfiles.rules = [ "d /var/lib/technitium 0755 nequi docker" ];
  };
}
