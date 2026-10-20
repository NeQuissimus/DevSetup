{
  config,
  lib,
  ...
}:
let
  allowed_domains = [
    "*.amazonaws.com" # AWS
    "api-us.roborock.com" # Roborock
    "awsusor0.fds.api.xiaomi.com" # Roborock
    "*.edgesuite.net" # Akamai CDN
    "ecsv2.roblox.com" # Roblox tracking
    "go.aftvnews.com" # AFTVNews Downloader
    "*.hf.co" # ML Models
    "huggingface.co" # ML Models
    "mqtt-mini.facebook.com" # Facebook messenger
    "mqtt-us.roborock.com" # Roborock
    "*.olg.ca" # OLG
    "*.tile-api.com" # Tile API
    "*.torproject.org" # TOR
    "tr.rbxcdn.com" # Roblox assets
    "track.spe.schoolmessenger.com" # School info
    "web.poecdn.com" # Path Of Exile website
  ];

  blocked_domains = [
    "api-fp-retry-bj.fengkongcloud.com" # Fingerprinting
  ];

  blocklists = [
    {
      description = "OISD Big";
      enabled = true;
      type = "block";
      url = "https://big.oisd.nl/";
    }
  ] ++ (lib.concatMap (
    # https://hagezi-mirror.dnsbunker.org/dlg.html?tif=full&bypass=doh&gambling=full&tiktok=standard&tier=proplus&items=nrd-7%2Cnrd-14-8%2Cnrd-21-15%2Cnrd-28-22%2Cnrd-35-29%2Choster%2Cdyndns%2Cnosafesearch%2Canti-piracy%2Cnsfw%2Cnative-amazon%2Cnative-apple%2Cnative-huawei%2Cnative-winoffice%2Cnative-samsung%2Cnative-lgwebos%2Cnative-roku%2Cnative-vivo%2Cnative-oppo-realme%2Cnative-xiaomi&tool=pi-hole
    # https://hagezi-mirror.dnsbunker.org/dlg.html?src=codeberg&tif=full&bypass=doh&gambling=full&tiktok=standard&tier=proplus&items=nrd-7%2Cnrd-14-8%2Cnrd-21-15%2Cnrd-28-22%2Cnrd-35-29%2Choster%2Cdyndns%2Cnosafesearch%2Canti-piracy%2Cnsfw%2Cnative-amazon%2Cnative-apple%2Cnative-huawei%2Cnative-winoffice%2Cnative-samsung%2Cnative-lgwebos%2Cnative-roku%2Cnative-vivo%2Cnative-oppo-realme%2Cnative-xiaomi&tool=pi-hole
    file: [
      {
        description = "Hazegi (dnsbunker) - ${file}";
        enabled = true;
        type = "block";
        url = "https://hagezi-mirror.dnsbunker.org/adblock/${file}";
      }
      {
        description = "Hazegi (codeberg) - ${file}";
        enabled = true;
        type = "block";
        url = "https://codeberg.org/hagezi/mirror2/raw/branch/main/dns-blocklists/adblock/${file}";
      }
    ]
  ) [
    "anti.piracy.txt"
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
    "nrd14-8.txt"
    "nrd21-15.txt"
    "nrd28-22.txt"
    "nrd35-29.txt"
    "nrd7.txt"
    "nsfw.txt"
    "pro.plus.txt"
    "tif.txt"
  ]
  ) ++ [
    {
      description = "Steven Black (github) - Hosts + Fakenews + Gambling + Porn";
      enabled = true;
      type = "block";
      url = "https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews-gambling-porn/hosts";
    }
    {
      description = "Steven Black (sbc) - Hosts + Fakenews + Gambling + Porn";
      enabled = true;
      type = "block";
      url = "http://sbc.io/hosts/alternates/fakenews-gambling-porn/hosts";
    }
  ];
in {  
  environment.etc."dns-allowlist.txt".text = builtins.concatStringsSep "\n" allowed_domains;
  environment.etc."dns-blocklist.txt".text = builtins.concatStringsSep "\n" blocked_domains;

  services = {
    pihole-ftl = {
      enable = true;

      lists = blocklists ++ [
        {
          description = "Custom allowlist";
          enabled = true;
          type = "allow";
          url = "/etc/dns-allowlist.txt";
        }
        {
          description = "Custom blocklist";
          enabled = true;
          type = "block";
          url = "/etc/dns-blocklist.txt";
        }
      ];

      openFirewallDNS = true;
      openFirewallWebserver = true;

      settings = {
        dns = {
          blockTTL = 3600; # Seconds

          cache = {
            size = 1000;
            optimizer = 7200; # Seconds
          };

          domain.name = "nequissimus.com";

          rateLimit = {
            count = 100;
            interval = 600; # Seconds
          };

          upstreams = [
            "94.140.14.15" # AdGuard Family Primary
            "94.140.15.16" # AdGuard Family Secondary
          ];
        };
      };
    };

    pihole-web = {
      enable = true;
      hostName = "${config.networking.hostName}.nequissimus.com";
      ports = [ "80r" "443s" ];
    };
  };
}