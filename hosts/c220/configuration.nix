{ config, pkgs, lib, ... }:
let
  interface = "enp4s0f0";

  dockerImages = {
    homeAssistant =
      "ghcr.io/home-assistant/home-assistant:2025.2@sha256:1191a95f9b82df94f467ad14dcb02bd6a5ddf244f8cf54a983c84a63bd612752";
    immich-ml =
      "ghcr.io/immich-app/immich-machine-learning:v1.125.7@sha256:5a7bac207c5be17bbe775fdca2fef7ec6635400180ae79cc7a41659cef2c05b0";
    matter =
      "ghcr.io/home-assistant-libs/python-matter-server:7.0.1@sha256:828c1cd3f957bb0287a099a439505457a25f5d65ed34281acf19cfbf537fe346";
    minecraft =
      "itzg/minecraft-server:2025.1.0-java17@sha256:d0e8397e16c3264d5eff0a5a17b6cd9d5a4a647216120d53502f1359b6864a91";
    musicAssistant =
      "ghcr.io/music-assistant/server:2.3.6@sha256:7c43aadfaf9109feab4c514648124701bf6b70410932ffcbf0c9daa7bdfbc2b2";
    seq =
      "datalust/seq:2024.3@sha256:4e81ce2e12c9086621f22ddc380c753129a7d6723b1a99eb8a43dbd2aa789e23";
    seq-parser =
      "smokserwis/seq-log-parser:latest@sha256:85cf07f5f8a988dfe1e4579a52ec773be947f247fecaed572c749bd7c575d97f";
    seq-syslog =
      "datalust/seq-input-syslog:1.0.93@sha256:a6da444b41e0c0ebae87dedb15ccbece27cb84605064b25984eba8d143fa12e0";
    technitium =
      "technitium/dns-server:13.4.1@sha256:2ecf0f90879f1a3b44ec9dcf6753327fd80ef7ab1d54659f9cd55df16fc4fd5a";
  };

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
    "https://gitlab.com/quidsup/notrack-annoyance-blocklist/-/raw/master/annoyance.hosts"
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

  allowed_domains = [
    "ecsv2.roblox.com" # Roblox tracking
    "huggingface.co" # ML Models
    "mqtt-mini.facebook.com" # Facebook messenger
    "mqtt-us.roborock.com" # Roborock message broker
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
in {
  imports = [
    ./hardware.nix

    ../../nixos/docker.nix
    ../../nixos/kernel.nix
    ../../nixos/nix.nix
    ../../nixos/security.nix
    ../../nixos/ssh.nix
    ../../nixos/users.nix
    ../../nixos/zfs.nix
    ../../nixos/zsh.nix
  ];

  console.keyMap = "us";

  documentation.nixos.enable = false;

  environment = {
    etc."minecraft/ops.json" = {
      text = ''
        [
          {
            "uuid": "106fcb90-9474-47d4-8663-58ffadbcef9a",
            "name": "thenequissimus",
            "level": 4,
            "bypassesPlayerLimit": true
          }
        ]
      '';
      mode = "0444";
    };

    etc."sysconfig/lm_sensors".text = ''
      HWMON_MODULES="coretemp"
    '';

    systemPackages = with pkgs; [ git htop ];
  };

  i18n = { defaultLocale = "en_US.UTF-8"; };

  networking = {
    defaultGateway = "10.0.0.2";

    firewall = {
      allowPing = true;
      allowedTCPPorts = [
        53 # Technitium
        1400 # Home Assistant
        3333 # Immich ML
        5080 # Seq
        5341 # Seq
        5342 # Seq
        5353 # Matter
        5380 # Technitium
        5540 # Matter
        5580 # Matter
        8095 # Music Assistant
        8096 # Music Assistant
        8097 # Music Assistant
        8123 # Home Assistant
        23565 # Minecraft
      ];
      allowedUDPPorts = [
        53 # DNS
        5514 # Seq Syslog
      ];
      enable = true;
    };

    hostId = "8990a246";
    hostName = "c220";

    interfaces."${interface}" = {
      ipv4.addresses = [{
        address = "10.0.0.52";
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

  powerManagement = {
    cpuFreqGovernor = "powersave";
    powertop.enable = true;
  };

  services = {
    avahi.enable = true;

    cron = {
      enable = true;
      systemCronJobs = [ "27 1 * * 0 root reboot" ];
    };

    logrotate.checkConfig =
      false; # https://github.com/NixOS/nixpkgs/pull/237414

    ntp = {
      enable = true;
      servers = [
        "0.ca.pool.ntp.org"
        "1.ca.pool.ntp.org"
        "2.ca.pool.ntp.org"
        "3.ca.pool.ntp.org"
      ];
    };

    ollama = {
      enable = true;

      environmentVariables = {
        OLLAMA_MAX_LOADED_MODELS = "1";
        OLLAMA_MAX_QUEUE = "2";
        OLLAMA_NUM_PARALLEL = "1";
      };

      home = "/var/lib/ollama";
      host = "0.0.0.0";

      loadModels = [ "qwen2.5:3b" "llava-phi3" ];

      openFirewall = true;
    };

    open-webui = {
      enable = true;

      environment = {
        ENABLE_COMMUNITY_SHARING = "False";
        ENABLE_MESSAGE_RATING = "False";
        ENABLE_MODEL_FILTER = "True";
        ENABLE_OPENAI_API = "False";
        ENABLE_SIGNUP = "False";
        MODEL_FILTER_LIST = "mistral:7b;qwen2.5:3b";
        OLLAMA_API_BASE_URL =
          "http://127.0.0.1:${toString config.services.ollama.port}";
        SAFE_MODE = "True";
        WEBUI_AUTH = "False";
      };

      host = "0.0.0.0";
      openFirewall = true;
      port = 3000;
    };

    openssh.enable = true;

    syslogd = {
      defaultConfig = "*.* @10.0.0.52:5514";

      enable = true;
    };

    thermald.enable = true;

    upower.enable = true;

    wyoming = {
      faster-whisper.servers."kevin" = {
        enable = true;
        language = "en";
        model = "tiny-int8";
        uri = "tcp://0.0.0.0:10300";
      };

      openwakeword = {
        customModelsDirectories = [ "/var/lib/openwakeword" ];

        enable = true;

        preloadModels = [ "ok_kevin" ];
      };

      piper.servers."kevin" = {
        enable = true;
        uri = "tcp://0.0.0.0:10200";
        voice = "en_GB-northern_english_male-medium";
      };
    };

    zfs.autoScrub = {
      enable = true;
      interval = "weekly";
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
          "curl http://localhost:5380/api/allowed/add?token=$TOKEN&domain=${x}")
        allowed_domains}
        ${lib.concatMapStringsSep "\n" (x:
          "curl http://localhost:5380/api/blocked/add?token=$TOKEN&domain=${x}")
        blocked_domains}
      '';
      wantedBy = [ "multi-user.target" ];
    };

    tmpfiles.rules = [
      "d /var/lib/homeassistant 0755 nequi docker"
      "d /var/lib/immich-ml 0755 nequi docker"
      "d /var/lib/matter 0755 nequi docker"
      "d /var/lib/mc 0755 nequi docker"
      "d /var/lib/mc2 0755 nequi docker"
      "d /var/lib/musicassistant 0755 nequi docker"
      "d /var/lib/seq 0755 nequi docker"
      "d /var/lib/technitium 0755 nequi docker"
      "L+ ${config.services.minecraft-server.dataDir}/ops.json - - - - /etc/minecraft/ops.json"
    ];
  };

  time = { timeZone = "America/Toronto"; };

  virtualisation.oci-containers = {
    backend = "docker";

    containers = {
      homeassistant = {
        autoStart = true;

        dependsOn = [ "matter" "musicassistant" "technitium" ];

        environment.TZ = "America/Toronto";

        extraOptions = [
          "--network=host"
          "--log-driver"
          "syslog"
          "--log-opt"
          "syslog-address=udp://10.0.0.52:5514"
          "--log-opt"
          "syslog-format=rfc5424"
        ];

        image = dockerImages.homeAssistant;
        volumes = [ "/var/lib/homeassistant:/config" ];
      };

      immich-ml = {
        autoStart = true;

        extraOptions = [
          "--log-driver"
          "syslog"
          "--log-opt"
          "syslog-address=udp://10.0.0.52:5514"
          "--log-opt"
          "syslog-format=rfc5424"
        ];

        image = dockerImages.immich-ml;
        ports = [ "3333:3003" ];
        volumes = [ "/var/lib/immich-ml:/cache" ];
      };

      matter = {
        autoStart = true;

        extraOptions = [
          "--network=host"
          "--log-driver"
          "syslog"
          "--log-opt"
          "syslog-address=udp://10.0.0.52:5514"
          "--log-opt"
          "syslog-format=rfc5424"
        ];

        image = dockerImages.matter;
        volumes = [ "/var/lib/matter:/data" ];
      };

      minecraftabe = {
        autoStart = true;

        # Start this one last
        dependsOn = [ "homeassistant" ];

        environment = {
          ALLOW_FLIGHT = "TRUE";
          ENABLE_ROLLING_LOGS = "TRUE";
          EULA = "TRUE";
          FTB_MODPACK_ID = "114";
          FTB_MODPACK_VERSION_ID = "12147";
          INIT_MEMORY = "2G";
          JVM_DD_OPTS = "disable.watchdog:true";
          MAX_MEMORY = "16G";
          MAX_PLAYERS = "4";
          MAX_TICK_TIME = "-1";
          MOTD = "Abe Pack 1.6.0";
          OPS = "thenequissimus,106fcb90-9474-47d4-8663-58ffadbcef9a";
          SERVER_NAME = "NeQuissimus - Abe Pack 1.6.0";
          SNOOPER_ENABLED = "FALSE";
          TYPE = "FTBA";
          USE_AIKAR_FLAGS = "TRUE";
        };

        extraOptions = [
          "--log-driver"
          "syslog"
          "--log-opt"
          "syslog-address=udp://10.0.0.52:5514"
          "--log-opt"
          "syslog-format=rfc5424"
        ];

        image = dockerImages.minecraft;
        ports = [ "23565:25565" ];
        user = "root";
        volumes = [ "/var/lib/mc2:/data" ];
      };

      musicassistant = {
        autoStart = true;
        environment = { LOG_LEVEL = "info"; };

        extraOptions = [
          "--network=host"
          "--log-driver"
          "syslog"
          "--log-opt"
          "syslog-address=udp://10.0.0.52:5514"
          "--log-opt"
          "syslog-format=rfc5424"
          "--cap-add=DAC_READ_SEARCH"
          "--cap-add=SYS_ADMIN"
          "--security-opt"
          "apparmor:unconfined"
        ];

        image = dockerImages.musicAssistant;
        volumes = [ "/var/lib/musicassistant:/data" ];
      };

      seq = {
        autoStart = true;

        environment = {
          ACCEPT_EULA = "y";
          SEQ_API_CANONICALURI = "http://10.0.0.52:5080";
        };

        image = dockerImages.seq;

        ports = [ "5080:80" "5341:5341" ];
        volumes = [ "/var/lib/seq:/data" ];
      };

      seq-parser = {
        autoStart = true;

        environment = {
          BIND_PORT = "5342";
          REGEX1 =
            "(?P<date>.*) (?P<timezone>[A-Z]{3}) (?P<source>[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}) (?P<cef>CEF:[0-9]*)\\|Ubiquiti\\|(?P<app>.*)\\|(?P<version>.*)\\|(?P<event_id>.*)\\|(?P<device>.*) was blocked from accessing (?P<target>[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}) by (?P<rule>.*)\\|(?P<level>.*)";
          REGEX2 =
            "(?P<date>.*) (?P<timezone>[A-Z]{3}) (?P<source>[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}) (?P<cef>CEF:[0-9]*)\\|Ubiquiti\\|(?P<app>.*)\\|(?P<version>.*)\\|(?P<event_id>.*)\\|(?P<message>.*)\\|(?P<level>.*)";
          SEQ_ADDRESS = "http://10.0.0.52:5341";
        };

        image = dockerImages.seq-parser;

        ports = [ "5342:5342" ];
      };

      seq-syslog = {
        autoStart = true;

        environment.SEQ_ADDRESS = "http://10.0.0.52:5342";

        image = dockerImages.seq-syslog;

        ports = [ "5514:514/udp" ];
      };

      technitium = {
        autoStart = true;

        environment = {
          DNS_SERVER_DOMAIN = "technitium";
          DNS_SERVER_ADMIN_PASSWORD = "admin";
          DNS_SERVER_WEB_SERVICE_LOCAL_ADDRESSES = "0.0.0.0";
          DNS_SERVER_WEB_SERVICE_ENABLE_HTTPS = "false";
          DNS_SERVER_ENABLE_BLOCKING = "true";
          DNS_SERVER_BLOCK_LIST_URLS = lib.concatStringsSep "," blocklists;
          DNS_SERVER_FORWARDERS = "9.9.9.11,149.112.112.11";
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
}

