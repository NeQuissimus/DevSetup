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

    systemPackages = with pkgs; [ git ];
  };

  i18n = { defaultLocale = "en_US.UTF-8"; };

  networking = {
    defaultGateway = "10.0.0.2";

    firewall = {
      allowPing = true;
      allowedTCPPorts = [
        80
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

  system.autoUpgrade = {
    channel = lib.mkDefault "https://nixos.org/channels/nixos-24.05";
    dates = "15:00";
    enable = true;
  };

  systemd = {
    tmpfiles.rules = [
      "d /var/lib/mc 0755 nequi docker"
      "d /var/lib/mc2 0755 nequi docker"
      "d /var/lib/pihole 0755 nequi docker"
      "d /var/lib/homeassistant 0755 nequi docker"
      "L+ ${config.services.minecraft-server.dataDir}/ops.json - - - - /etc/minecraft/ops.json"
    ];
  };

  time = { timeZone = "America/Toronto"; };

  virtualisation.oci-containers = {
    backend = "docker";

    containers = {
      homeassistant = {
        autoStart = true;
        volumes = [ "/var/lib/homeassistant:/config" ];
        environment.TZ = "America/Toronto";
        image =
          "ghcr.io/home-assistant/home-assistant:stable@sha256:408a5a63e3e9a89ceb6ecd98345e54c86073314b4d94e217cd54f7208307406d";
        extraOptions = [ "--network=host" ];
      };

      minecraftabe = {
        autoStart = true;
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
    };
  };
}

