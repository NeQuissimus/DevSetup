{ config, pkgs, lib, ... }:
let
  interface = "enp4s0f0";
  dockerImages = {
    homeAssistant =
      "ghcr.io/home-assistant/home-assistant:2024.12.4@sha256:093f4255f1cd1bddabadfb109d5dbf56f87aaa2f419f6c0377f947ed0ab02204";
    matter =
      "ghcr.io/home-assistant-libs/python-matter-server:6.6.1@sha256:2057a36093e8a0e5a9d6c391a2be64401944783a6263e26c992b7790033304b5";
    minecraft =
      "itzg/minecraft-server:java17@sha256:3a50610894211a5bc7075b853372f8803065067a41d7965b177b297f5bb543a8";
    musicAssistant =
      "ghcr.io/music-assistant/server:2.3.3@sha256:c850589a57abacc835eaccc99fb784d68c3c4876ebc7bccf356a2e3fff6b5ed6";
    ollama =
      "ollama/ollama:0.5.4@sha256:18bfb1d605604fd53dcad20d0556df4c781e560ebebcd923454d627c994a0e37";
    openWebui =
      "ghcr.io/open-webui/open-webui:0.4.8@sha256:c1e4f0927fb0acd53bcbd8bbc92ecaf7ca36a23ee4cb8f25ce90c541012a473a";
    openwakeword =
      "rhasspy/wyoming-openwakeword:1.10.0@sha256:3165a5cd8aef84beb882e640aa1f5c01c97f8c0b1f50016164ecdf2ab65d033a";
    pihole =
      "pihole/pihole:2024.07.0@sha256:0def896a596e8d45780b6359dbf82fc8c75ef05b97e095452e67a0a4ccc95377";
    piper =
      "rhasspy/wyoming-piper:1.5.0@sha256:b6bf208855f26168790ed336ad16576b2fb290f31b51fb98aca496a45561516f";
    whisper =
      "rhasspy/wyoming-whisper:2.4.0@sha256:2687f79715734606f856fc4478351ce91a76bdaf6899086bcb837eb9ac7cddb8";
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
        80
        1400
        3000
        8095
        8097
        8123
        config.services.minecraft-server.serverProperties.server-port
      ];
      allowedUDPPorts =
        [ config.services.minecraft-server.serverProperties.server-port ];
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
        ''
          0 * * * 0 nequi ${pkgs.curl}/bin/curl http://localhost:11434/api/chat -d '{"model": "qwen2.5:3b", "keep_alive": -1}' ''
      ];
    };

    logrotate.checkConfig =
      false; # https://github.com/NixOS/nixpkgs/pull/237414

    minecraft-server = {
      dataDir = "/var/lib/minecraft";
      declarative = true;
      enable = false;
      eula = true;

      jvmOpts =
        "-Xms2G -Xmx8G -XX:+UseG1GC -XX:+ParallelRefProcEnabled -XX:MaxGCPauseMillis=200 -XX:+UnlockExperimentalVMOptions -XX:+UnlockDiagnosticVMOptions -XX:+DisableExplicitGC -XX:+AlwaysPreTouch -XX:G1NewSizePercent=30 -XX:G1MaxNewSizePercent=40 -XX:G1HeapRegionSize=8M -XX:G1ReservePercent=20 -XX:G1HeapWastePercent=5 -XX:G1MixedGCCountTarget=4 -XX:InitiatingHeapOccupancyPercent=15 -XX:G1MixedGCLiveThresholdPercent=90 -XX:G1RSetUpdatingPauseTimePercent=5 -XX:SurvivorRatio=32 -XX:+PerfDisableSharedMem -XX:MaxTenuringThreshold=1 -XX:-UseBiasedLocking -XX:UseAVX=3 -XX:+UseStringDeduplication -XX:+UseFastUnorderedTimeStamps -XX:+UseAES -XX:+UseAESIntrinsics -XX:UseSSE=4 -XX:+UseFMA -XX:AllocatePrefetchStyle=1 -XX:+UseLoopPredicate -XX:+RangeCheckElimination -XX:+EliminateLocks -XX:+DoEscapeAnalysis -XX:+UseCodeCacheFlushing -XX:+SegmentedCodeCache -XX:+UseFastJNIAccessors -XX:+OptimizeStringConcat -XX:+UseCompressedOops -XX:+UseThreadPriorities -XX:+OmitStackTraceInFastThrow -XX:+TrustFinalNonStaticFields -XX:ThreadPriorityPolicy=1 -XX:+UseInlineCaches -XX:+RewriteBytecodes -XX:+RewriteFrequentPairs -XX:+UseNUMA -XX:-DontCompileHugeMethods -XX:+UseFPUForSpilling -XX:+UseFastStosb -XX:+UseNewLongLShift -XX:+UseVectorCmov -XX:+UseXMMForArrayCopy -XX:+UseXmmI2D -XX:+UseXmmI2F -XX:+UseXmmLoadAndClearUpper -XX:+UseXmmRegToRegMoveAll -Dfile.encoding=UTF-8 -Xlog:async -Djava.security.egd=file:/dev/urandom";

      package = pkgs.minecraftServers.vanilla-1-19;

      serverProperties = {
        allow-flight = true;
        difficulty = 2;
        enforce-whitelist = true;
        level-seed = "5743982194324514800";
        max-players = 5;
        max-tick-time = 120000;
        max-world-size = 15000;
        motd = "NeQuissimus MC";
        pvp = false;
        server-port = 25565;
        snooper-enabled = false;
        spawn-protection = 0;
        white-list = true;
      };

      whitelist = { germadian = "582bf397-4392-48b5-b7c1-7511aa15d51e"; };
    };

    ntp = {
      enable = true;
      servers = [
        "0.ca.pool.ntp.org"
        "1.ca.pool.ntp.org"
        "2.ca.pool.ntp.org"
        "3.ca.pool.ntp.org"
      ];
    };

    openssh.enable = true;

    thermald.enable = true;

    upower.enable = true;

    zfs.autoScrub = {
      enable = true;
      interval = "weekly";
    };
  };

  systemd = {
    tmpfiles.rules = [
      "d /var/lib/homeassistant 0755 nequi docker"
      "d /var/lib/matter 0755 nequi docker"
      "d /var/lib/mc 0755 nequi docker"
      "d /var/lib/mc2 0755 nequi docker"
      "d /var/lib/musicassistant 0755 nequi docker"
      "d /var/lib/ollama 0755 nequi docker"
      "d /var/lib/openwakeword 0755 nequi docker"
      "d /var/lib/open-webui 0755 nequi docker"
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

        dependsOn = [
          "matter"
          "musicassistant"
          "ollama"
          "openwakeword"
          "pihole"
          "piper"
          "whisper"
        ];

        environment.TZ = "America/Toronto";
        extraOptions = [ "--network=host" ];
        image = dockerImages.homeAssistant;
        volumes = [ "/var/lib/homeassistant:/config" ];
      };

      matter = {
        autoStart = true;
        volumes = [ "/var/lib/matter:/data" ];
        image = dockerImages.matter;
        extraOptions = [ "--network=host" ];
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

      ollama = {
        autoStart = true;
        environment = {
          OLLAMA_MAX_LOADED_MODELS = "1";
          OLLAMA_MAX_QUEUE = "2";
          OLLAMA_NUM_PARALLEL = "1";
        };
        image = dockerImages.ollama;
        ports = [ "11434:11434" ];
        volumes = [ "/var/lib/ollama:/root/.ollama" ];
      };

      open-webui = {
        autoStart = true;
        dependsOn = [ "ollama" ];
        environment = {
          ENABLE_COMMUNITY_SHARING = "False";
          ENABLE_MESSAGE_RATING = "False";
          ENABLE_MODEL_FILTER = "True";
          ENABLE_OPENAI_API = "False";
          ENABLE_SIGNUP = "False";
          MODEL_FILTER_LIST = "mistral:7b;qwen2.5:3b";
          PORT = "3000";
          SAFE_MODE = "True";
          WEBUI_AUTH = "False";
        };

        extraOptions =
          [ "--network=host" "--add-host=host.docker.internal:host-gateway" ];
        image = dockerImages.openWebui;
        volumes = [ "/var/lib/open-webui:/app/backend/data" ];
      };

      openwakeword = {
        autoStart = true;
        cmd = [ "--custom-model-dir" "/custom" "--preload-model" "hey_jarvis" ];
        image = dockerImages.openwakeword;
        ports = [ "10400:10400" ];
        volumes = [ "/var/lib/openwakeword:/custom" ];
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

      piper = {
        autoStart = true;
        cmd = [ "--voice" "en_GB-northern_english_male-medium" ];
        image = dockerImages.piper;
        ports = [ "10200:10200" ];
      };

      whisper = {
        autoStart = true;
        cmd = [ "--model" "tiny-int8" "--language" "en" ];
        image = dockerImages.whisper;
        ports = [ "10300:10300" ];
      };
    };
  };
}

