{ config, pkgs, lib, ... }:
let interface = "enp4s0f0";
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
        image =
          "ghcr.io/home-assistant/home-assistant:2024.12.1@sha256:ec483debb415123b5ebe49b9e6b5021d055e5f62c832acc094503afa09bb448d";
        volumes = [ "/var/lib/homeassistant:/config" ];
      };

      matter = {
        autoStart = true;
        volumes = [ "/var/lib/matter:/data" ];
        image =
          "ghcr.io/home-assistant-libs/python-matter-server:stable@sha256:e291154e44accc5284ec14fbc52a32764a8286deb68b4d3b91b280ad644715d8";
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
          MAX_MEMORY = "8G";
          MAX_PLAYERS = "4";
          MAX_TICK_TIME = "-1";
          MOTD = "Abe Pack 1.6.0";
          OPS = "thenequissimus,106fcb90-9474-47d4-8663-58ffadbcef9a";
          SERVER_NAME = "NeQuissimus - Abe Pack 1.6.0";
          SNOOPER_ENABLED = "FALSE";
          TYPE = "FTBA";
          USE_AIKAR_FLAGS = "TRUE";
        };
        image =
          "itzg/minecraft-server:java17@sha256:b673a66b9cbc5de6eb278f044bbe0ac12ec47d8d49330d9c58770ebb2427c936";
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

        image =
          "ghcr.io/music-assistant/server:2.3.3@sha256:c850589a57abacc835eaccc99fb784d68c3c4876ebc7bccf356a2e3fff6b5ed6";
        volumes = [ "/var/lib/musicassistant:/data" ];
      };

      ollama = {
        autoStart = true;
        environment = {
          OLLAMA_MAX_LOADED_MODELS = "1";
          OLLAMA_MAX_QUEUE = "2";
          OLLAMA_NUM_PARALLEL = "1";
        };
        image =
          "ollama/ollama:0.5.1@sha256:722ce8caba5f8b8bd2ee654b2e29466415be3071a704e3f4db1702b83c885f76";
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
        image =
          "ghcr.io/open-webui/open-webui:0.4.8@sha256:c1e4f0927fb0acd53bcbd8bbc92ecaf7ca36a23ee4cb8f25ce90c541012a473a";
        volumes = [ "/var/lib/open-webui:/app/backend/data" ];
      };

      openwakeword = {
        autoStart = true;
        cmd = [ "--custom-model-dir" "/custom" "--preload-model" "hey_jarvis" ];
        image =
          "rhasspy/wyoming-openwakeword:1.10.0@sha256:3165a5cd8aef84beb882e640aa1f5c01c97f8c0b1f50016164ecdf2ab65d033a";
        ports = [ "10400:10400" ];
        volumes = [ "/var/lib/openwakeword:/custom" ];
      };

      pihole = {
        autoStart = true;
        environment = {
          TZ = "America/Toronto";
          WEBPASSWORD = "admin"; # This is fine
        };
        image =
          "pihole/pihole:latest@sha256:e53305e9e00d7ac283763ca9f323cc95a47d0113a1e02eb9c6849f309d6202dd";
        ports = [ "53:53/tcp" "53:53/udp" "80:80/tcp" ];
        volumes = [
          "/var/lib/pihole/etc-pihole:/etc/pihole"
          "/var/lib/pihole/etc-dnsmasq.d:/etc/dnsmasq.d"
        ];
      };

      piper = {
        autoStart = true;
        cmd = [ "--voice" "en_GB-northern_english_male-medium" ];
        image =
          "rhasspy/wyoming-piper:1.5.0@sha256:b6bf208855f26168790ed336ad16576b2fb290f31b51fb98aca496a45561516f";
        ports = [ "10200:10200" ];
      };

      whisper = {
        autoStart = true;
        cmd = [ "--model" "tiny-int8" "--language" "en" ];
        image =
          "rhasspy/wyoming-whisper:2.2.0@sha256:49d07d5d5ef10b27e228810426d94d3500555ba8ca619485dde4714d8ae85762";
        ports = [ "10300:10300" ];
      };
    };
  };
}

