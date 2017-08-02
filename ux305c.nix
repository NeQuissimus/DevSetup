{ config, lib, pkgs, ... }:

{
  imports = [ ./ux305c-hardware.nix ./xmonad-config.nix ./ux305c-wifi.nix ];

  boot = {
    cleanTmpDir = true;

    initrd.kernelModules = ["ahci" "aesni-intel"];

    kernel.sysctl = {
      "vm.dirty_writeback_centisecs" = 1500;
      "vm.drop_caches" = 3;
      "vm.laptop_mode" = 5;
      "vm.swappiness" = 1;
    };

    kernelPackages = pkgs.linuxPackages_hardened;

    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
    };
  };

  environment = {
    sessionVariables = {
      TERMINFO_DIRS = "/run/current-system/sw/share/terminfo";
    };

    systemPackages = with pkgs; [
      # Basics
      alacritty
      atom
      autocutsel
      binutils
      chromium
      conky
      dmenu
      gitFull
      gnupg1compat
      htop
      i3lock-fancy
      jq
      oh-my-zsh
      skopeo
      upower
    ];
  };

  fonts = {
    enableFontDir = true;

    fonts = with pkgs; [
      dejavu_fonts
      font-awesome-ttf
      nerdfonts
      source-code-pro
    ];

    fontconfig.defaultFonts.monospace = [ "DejaVu Sans Mono" ];
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    pulseaudio.enable = true;
  };

  i18n = {
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  networking = {
    enableIPv6 = true;

    extraHosts = (lib.fileContents ./hosts);

    hostName = "nixus";

    firewall = {
      allowedTCPPorts = [ 22 ];
      allowPing = false;
      enable = true;
    };

    hosts = {
      "127.0.0.1" = ["${config.networking.hostName}" "localhost"];
      "0.0.0.0" = ["ftp.au.debian.org"];
    };
  };

  nix = {
    binaryCaches = [ https://cache.nixos.org ];
    buildCores = 8;

    extraOptions = ''
      auto-optimise-store = true
      binary-caches-parallel-connections = 20
      connect-timeout = 10
    '';

    gc = {
      automatic = true;
      dates = "18:00";
      options = "--delete-older-than 30";
    };

    maxJobs = 4;

    nrBuildUsers = 30;
    trustedBinaryCaches = [ https://cache.nixos.org ];
    useSandbox = true;
  };

  nixpkgs.config.packageOverrides = pkgs:
    { virtualbox = pkgs.virtualbox.override { enable32bitGuests = false; };
  };

  programs = {
    chromium = {
      enable = true;

      defaultSearchProviderSearchURL = ''https://encrypted.google.com/search?q={searchTerms}&{google:RLZ}{google:originalQueryForSuggestion}{google:assistedQueryStats}{google:searchFieldtrialParameter}{google:searchClient}{google:sourceId}{google:instantExtendedEnabledParameter}ie={inputEncoding}'';

      extensions = [
        "obdbgnebcljmgkoljcdddaopadkifnpm" # Canvas Defender
        "kbfnbcaeplbcioakkpcpgfkobkghlhen" # Grammarly
        "ehhkfhegcenpfoanmgfpfhnmdmflkbgk" # Home - New Tab Page
        "gcbommkclmclpchllfjekcdonpmejbdp" # HTTPS Everywhere
        "pkehgijcmpdhfbdbbnkijodmdjhbjlgp" # Privacy Badger
        "niloccemoadcdkdjlinkgdfekeahmflj" # Save to Pocket
        "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
        "dbepggeogbaibhgnhhndojpepiihcmeb" # Vimium
      ];
    };

    ssh = {
      agentTimeout = "4h";
      extraConfig = ''
        Host *
          ConnectTimeout 60
          ServerAliveInterval 240
          ConnectionAttempts 60
      '';
      knownHosts = [
        {
          hostNames = [ "pine" "pine.nequissimus.com" "10.30.27.128" ];
          publicKey = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBLd5r3p3khAwHU8h2W/vCk872XV9iOqL3GvwofWkw5xbtljG1AnTPeBjxHVAaQCagrrJGA9iGope2g6A6ydpvn4=";
        }
        {
          hostNames = [ "github.com" "192.30.252.*" "192.30.253.*" "192.30.254.*" "192.30.255.*" ];
          publicKey = "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==";
        }
      ];
      startAgent = true;
    };

    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;

      ohMyZsh = {
        enable = true;
      };

      promptInit = ''
        autoload -U promptinit && promptinit && prompt clint
      '' + (lib.fileContents ./_home/zshrc);

      syntaxHighlighting = {
        enable = true;

        highlighters = [ "main" "brackets" "pattern" "cursor" "root" "line"];
      };
    };
  };

  security = {
    chromiumSuidSandbox.enable = false;

    hideProcessInformation = true;

    sudo = {
      enable = true;
      wheelNeedsPassword = true;
    };
  };

  services = {
    locate.enable = true;

    nixosManual.enable = false;

    ntp = {
      enable = true;
      servers = [ "0.ca.pool.ntp.org" "1.ca.pool.ntp.org" "2.ca.pool.ntp.org" "3.ca.pool.ntp.org" ];
    };

    redshift = {
      enable = true;
      latitude = "43.18";
      longitude = "-80.38";
      temperature.night = 1900;
    };

    tlp.enable = true;

    upower.enable = true;

    xserver = {
      autorun = true;
      defaultDepth = 24;
      displayManager = {
        sessionCommands = with pkgs; lib.mkAfter ''
          ${xorg.xsetroot}/bin/xsetroot -solid "#222222" &
          ${xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr &
          ${autocutsel}/bin/autocutsel &
          ${autocutsel}/bin/autocutsel -s PRIMARY &
        '';
        slim.enable = true;
        xserverArgs = [ "-logfile" "/var/log/X.log" ];
      };
      enable = true;
      exportConfiguration = true;
      resolutions = [{x = 1920; y = 1080;} {x = 1280; y = 800;} {x = 1024; y = 768;}];
      synaptics = {
        enable = true;
        tapButtons = false;
        twoFingerScroll = true;
      };
      videoDriver = "intel";
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
      xkbOptions = "ctrl:nocaps";
    };
  };

  system = {
    autoUpgrade = {
      channel = "https://nixos.org/channels/nixos-unstable-small";
      dates = "19:00";
      enable = true;
    };
    stateVersion = "17.03";
  };

  time = {
    timeZone = "America/Toronto";
  };

  users = {
    defaultUserShell = "${pkgs.zsh}/bin/zsh";

    extraUsers.nequi = {
     createHome = true;
     extraGroups = [ "docker" "rkt" "wheel" ];
     group = "users";
     home = "/home/nequi";
     name = "nequi";
     uid = 1000;
     useDefaultShell = true;

     openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA7Jdj3a0bXoMTTE7dTLtAuB3aY5ZCTvWGhmlYYYFC/D timsteinbach@iPixel.local"
     ];
    };
  };

  virtualisation = {
    docker = {
      enable = false;
      storageDriver = "btrfs";
    };

    rkt.enable = false;

    virtualbox.host.enable = false;
  };
}
