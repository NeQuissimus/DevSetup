{ config, pkgs, lib, ... }:
let
  interface = "enp4s0f0";

  dockerImages = {
    homeAssistant =
      "ghcr.io/home-assistant/home-assistant:2025.6.3@sha256:e207929bdf5dc95db43c618b877364e99f7ad506ec5440aeef80d5c9c1cae668";
    immich-ml =
      "ghcr.io/immich-app/immich-machine-learning:v1.135.3@sha256:9f2f61d86af82d04926f9b896c995c502303052905517c5485dd26bf1e42a44e";
    matter =
      "ghcr.io/home-assistant-libs/python-matter-server:8.0.0@sha256:8fd1ea29ab5eca1c5e87cb983c9797b469ad315f6667c73a28b2c4c23a75923c";
    minecraft =
      "itzg/minecraft-server:2025.6.0-java17@sha256:626aade711a27c1ba9dec1faaa326e5baeeea5cff19db72a0129723edec1a456";
    musicAssistant =
      "ghcr.io/music-assistant/server:2.5.5@sha256:1d3527bb2d217634e875d6cdb6e459f6ff9dcde02be60c9a02fdd8dd86648f78";
    seq =
      "datalust/seq:2025.2.14199@sha256:ca47ade2527cb167f31c310f5530e1a2d8d801ce5ff6b2f3deed53b42da7434e";
    seq-parser =
      "smokserwis/seq-log-parser:latest@sha256:85cf07f5f8a988dfe1e4579a52ec773be947f247fecaed572c749bd7c575d97f";
    seq-syslog =
      "datalust/seq-input-syslog:1.0.93@sha256:a6da444b41e0c0ebae87dedb15ccbece27cb84605064b25984eba8d143fa12e0";
    speech-to-phrase =
      "rhasspy/wyoming-speech-to-phrase:1.3.0@sha256:d475fa739caeaf890dbcc41e134fef0fa18013ebd23b096069b90cc9d522a01e";
  };

  hassToken = lib.trim (builtins.readFile (/etc/hass/hass_token));
in {
  imports = [
    ./hardware.nix

    ../../nixos/dns.nix
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
      systemCronJobs = [
        "0 16 * * 1,2,3,4,5 root systemctl start docker-minecraftabe.service"
        "0 23 * * 1,2,3,4,5 root systemctl stop docker-minecraftabe.service"
        "0  9 30 * 6,0       root systemctl start docker-minecraftabe.service"
        "0 23 * * 6,0       root systemctl stop docker-minecraftabe.service"

        "0 9 * * 0 root reboot"
      ];
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
        OLLAMA_NOHISTORY = "true";
        OLLAMA_NUM_PARALLEL = "1";
      };

      home = "/var/lib/ollama";
      host = "0.0.0.0";

      loadModels = [
        "gemma3:1b"
        "gemma3:4b"
        "gemma3:1b-it-qat"
        "gemma3:4b-it-qat"
        "qwen2.5vl:3b"
      ];

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
        MODEL_FILTER_LIST =
          "gemma3:4b;gemma3:1b;gemma3:1b-it-qat;gemma3:4b-it-qat;qwen2.5vl:3b";
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

        preloadModels = [ "hey_jarvis" ];
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

  system.autoUpgrade.channel = "https://nixos.org/channels/nixos-unstable";

  systemd.tmpfiles.rules = [
    "d /var/lib/homeassistant 0755 nequi docker"
    "d /var/lib/immich-ml 0755 nequi docker"
    "d /var/lib/matter 0755 nequi docker"
    "d /var/lib/mc 0755 nequi docker"
    "d /var/lib/mc2 0755 nequi docker"
    "d /var/lib/musicassistant 0755 nequi docker"
    "d /var/lib/seq 0755 nequi docker"
    "d /var/lib/speech-to-phrase 0755 nequi docker"
    "L+ ${config.services.minecraft-server.dataDir}/ops.json - - - - /etc/minecraft/ops.json"
  ];

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

        dependsOn = [ "technitium" ];

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
        dependsOn = [ "technitium" ];
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

        dependsOn = [ "seq" ];

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

        dependsOn = [ "seq" ];

        environment.SEQ_ADDRESS = "http://10.0.0.52:5342";

        image = dockerImages.seq-syslog;

        ports = [ "5514:514/udp" ];
      };

      speech-to-phrase = {
        autoStart = true;

        cmd = [
          "--hass-websocket-uri"
          "ws://10.0.0.52:8123/api/websocket"
          "--hass-token"
          "${hassToken}"
          "--retrain-on-start"
        ];

        dependsOn = [ "homeassistant" ];

        extraOptions = [
          "--log-driver"
          "syslog"
          "--log-opt"
          "syslog-address=udp://10.0.0.52:5514"
          "--log-opt"
          "syslog-format=rfc5424"
        ];

        image = dockerImages.speech-to-phrase;

        ports = [ "10500:10300" ];
        volumes = [
          "/var/lib/speech-to-phrase/models:/models"
          "/var/lib/speech-to-phrase/train:/train"
        ];
      };
    };
  };
}
