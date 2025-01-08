{ config, pkgs, lib, ... }:
let
  interface = "enp4s0f0";
  dockerImages = {
    homeAssistant =
      "ghcr.io/home-assistant/home-assistant:2025.1@sha256:7db850eff6b858b6d01860cd76a10d993861f9bff140de85734ce01d153a62ca";
    immich-ml =
      "ghcr.io/immich-app/immich-machine-learning:v1.123.0@sha256:fca90362ff3081fc7762d731eb24de262181eaec28afc51eff1d3ca5348663cd";
    matter =
      "ghcr.io/home-assistant-libs/python-matter-server:7.0.1@sha256:828c1cd3f957bb0287a099a439505457a25f5d65ed34281acf19cfbf537fe346";
    minecraft =
      "itzg/minecraft-server:java17@sha256:21496fbcfee0eaf149b85e04d0e02e5186ebb8266ea4f806bcf0500cccbd8174";
    musicAssistant =
      "ghcr.io/music-assistant/server:2.3.5@sha256:c9bd5dd2d1f3741649e5398c472b43fdb1c68ef69c8f8d0e0dd261c84cf0d3c1";
    technitium =
      "technitium/dns-server:13.3.0@sha256:4acc49f3cf01f6ab405332d1a2ce0a8c512007014d73a03013c17616b446095b";
  };
in {
  imports = [
    ./c220-hardware.nix

    ./nixos/docker.nix
    ./nixos/kernel.nix
    ./nixos/nix.nix
    ./nixos/security.nix
    ./nixos/ssh.nix
    ./nixos/users.nix
    ./nixos/zfs.nix
    ./nixos/zsh.nix
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
      allowedUDPPorts = [ 53 ];
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
        export TOKEN=$(</var/lib/technitium/token) && \
        curl "http://localhost:5380/api/allowed/add?token=$TOKEN&domain=olg.ca" && \
        curl "http://localhost:5380/api/allowed/add?token=$TOKEN&domain=mqtt-mini.facebook.com" && \
        curl "http://localhost:5380/api/allowed/add?token=$TOKEN&domain=transport.home.nest.com" && \
        curl "http://localhost:5380/api/allowed/add?token=$TOKEN&domain=mqtt-us.roborock.com" && \
        curl "http://localhost:5380/api/allowed/add?token=$TOKEN&domain=tile-api.com"
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
        extraOptions = [ "--network=host" ];
        image = dockerImages.homeAssistant;
        volumes = [ "/var/lib/homeassistant:/config" ];
      };

      immich-ml = {
        autoStart = true;
        image = dockerImages.immich-ml;
        ports = [ "3333:3003" ];
        volumes = [ "/var/lib/immich-ml:/cache" ];
      };

      matter = {
        autoStart = true;
        extraOptions = [ "--network=host" ];
        image = dockerImages.matter;
        volumes = [ "/var/lib/matter:/data" ];
      };

      minecraftabe = {
        autoStart = true;

        # Start this one last
        dependsOn = [ "homeassistant" ];

        environment = {
          ALLOW_FLIGHT = "TRUE";
          AUTOPAUSE_KNOCK_INTERFACE = "eth0";
          AUTOPAUSE_TIMEOUT_EST = "60";
          AUTOPAUSE_TIMEOUT_INIT = "60";
          ENABLE_AUTOPAUSE = "TRUE";
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
          "--cap-add=DAC_READ_SEARCH"
          "--cap-add=SYS_ADMIN"
          "--security-opt"
          "apparmor:unconfined"
        ];

        image = dockerImages.musicAssistant;
        volumes = [ "/var/lib/musicassistant:/data" ];
      };

      technitium = {
        autoStart = true;

        environment = {
          DNS_SERVER_DOMAIN = "technitium";
          DNS_SERVER_ADMIN_PASSWORD = "admin";
          DNS_SERVER_WEB_SERVICE_LOCAL_ADDRESSES = "0.0.0.0";
          DNS_SERVER_WEB_SERVICE_ENABLE_HTTPS = "false";
          DNS_SERVER_ENABLE_BLOCKING = "true";
          DNS_SERVER_BLOCK_LIST_URLS =
            "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts,https://raw.githubusercontent.com/laylavish/uBlockOrigin-HUGE-AI-Blocklist/main/noai_hosts.txt,https://blocklistproject.github.io/Lists/smart-tv.txt,https://someonewhocares.org/hosts/zero/hosts,https://perflyst.github.io/PiHoleBlocklist/AmazonFireTV.txt,https://perflyst.github.io/PiHoleBlocklist/android-tracking.txt,https://gitlab.com/quidsup/notrack-annoyance-blocklist/-/raw/master/annoyance.hosts,https://github.com/hagezi/dns-blocklists/raw/refs/heads/main/hosts/ultimate.txt,https://raw.githubusercontent.com/hagezi/dns-blocklists/main/hosts/tif.txt,https://raw.githubusercontent.com/xRuffKez/NRD/refs/heads/main/lists/30-day/domains-only/nrd-30day_part1.txt,https://raw.githubusercontent.com/xRuffKez/NRD/refs/heads/main/lists/30-day/domains-only/nrd-30day_part2.txt,https://raw.githubusercontent.com/hagezi/dns-blocklists/main/hosts/doh.txt,https://raw.githubusercontent.com/hagezi/dns-blocklists/main/adblock/gambling.medium.txt,https://blocklistproject.github.io/Lists/abuse.txt,https://blocklistproject.github.io/Lists/ads.txt,https://blocklistproject.github.io/Lists/fraud.txt,https://blocklistproject.github.io/Lists/gambling.txt,https://blocklistproject.github.io/Lists/malware.txt,https://blocklistproject.github.io/Lists/phishing.txt,https://blocklistproject.github.io/Lists/porn.txt,https://blocklistproject.github.io/Lists/scam.txt,https://blocklistproject.github.io/Lists/tracking.txt";
          DNS_SERVER_FORWARDERS = "9.9.9.11,149.112.112.11";
          DNS_SERVER_LOG_USING_LOCAL_TIME = "true";
        };

        image = dockerImages.technitium;
        ports = [ "53:53/tcp" "53:53/udp" "5380:5380/tcp" ];
        volumes = [ "/var/lib/technitium:/etc/dns" ];
      };
    };
  };
}

