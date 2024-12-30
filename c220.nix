{ config, pkgs, lib, ... }:
let
  interface = "enp4s0f0";
  dockerImages = {
    homeAssistant =
      "ghcr.io/home-assistant/home-assistant:2024.12.5@sha256:132ef461504be5c5ebd6e34e5d3fb3d7958bb6758a5136107eea9f84c299254a";
    immich-ml =
      "ghcr.io/immich-app/immich-machine-learning:v1.123.0@sha256:fca90362ff3081fc7762d731eb24de262181eaec28afc51eff1d3ca5348663cd";
    matter =
      "ghcr.io/home-assistant-libs/python-matter-server:7.0.0@sha256:1e371d6936c179fec67896180a8697448c2ccec628078f709bb4b271b040701b";
    minecraft =
      "itzg/minecraft-server:java17@sha256:3a50610894211a5bc7075b853372f8803065067a41d7965b177b297f5bb543a8";
    musicAssistant =
      "ghcr.io/music-assistant/server:2.3.4@sha256:301cc44d2405e1f12953a44ddb65454630007755af9ada9d204bc1a3b9a06175";
    pihole =
      "pihole/pihole:2024.07.0@sha256:0def896a596e8d45780b6359dbf82fc8c75ef05b97e095452e67a0a4ccc95377";
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
        53 # Pihole
        80 # Pihole
        1400 # Home Assistant
        3333 # Immich ML
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
    tmpfiles.rules = [
      "d /var/lib/homeassistant 0755 nequi docker"
      "d /var/lib/immich-ml 0755 nequi docker"
      "d /var/lib/matter 0755 nequi docker"
      "d /var/lib/mc 0755 nequi docker"
      "d /var/lib/mc2 0755 nequi docker"
      "d /var/lib/musicassistant 0755 nequi docker"
      "d /var/lib/pihole 0755 nequi docker"
      "L+ ${config.services.minecraft-server.dataDir}/ops.json - - - - /etc/minecraft/ops.json"
    ];
  };

  time = { timeZone = "America/Toronto"; };

  virtualisation.oci-containers = {
    backend = "docker";

    containers = {
      homeassistant = {
        autoStart = true;

        dependsOn = [ "matter" "musicassistant" "pihole" ];

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

      pihole = {
        autoStart = true;
        environment = {
          TZ = "America/Toronto";
          WEBPASSWORD = "admin"; # This is fine
        };
        image = dockerImages.pihole;
        ports = [ "53:53/tcp" "53:53/udp" "80:80/tcp" ];
        volumes = [
          "/var/lib/pihole/etc-pihole:/etc/pihole"
          "/var/lib/pihole/etc-dnsmasq.d:/etc/dnsmasq.d"
        ];
      };
    };
  };
}

