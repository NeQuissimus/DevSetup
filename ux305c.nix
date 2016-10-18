{ config, lib, pkgs, ... }:

{
  imports = [ ./ux305c-hardware.nix ./ux305c-wifi.nix ];

  boot = {
    cleanTmpDir = true;

    initrd.kernelModules = ["ahci" "aesni-intel"];

    kernel.sysctl = {
      "vm.dirty_writeback_centisecs" = 1500;
      "vm.drop_caches" = 1;
      "vm.laptop_mode" = 5;
      "vm.swappiness" = 5;
    };

    kernelPackages = pkgs.linuxPackages_latest;

    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    # Basics
    binutils
    conky
    htop
    i3lock-fancy
    parcellite
    upower
  ];

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      font-awesome-ttf
      source-code-pro
    ];
    fontconfig.defaultFonts.monospace = [ "Source Code Pro" ];
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    pulseaudio.enable = false;
  };

  i18n = {
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  networking = {
    hostName = "nixus";

    extraHosts = ''
      127.0.0.1 nixus
      0.0.0.0 ftp.au.debian.org
    '' + (lib.fileContents ./hosts);

    firewall = {
      allowedTCPPorts = [ 22 ];
      allowPing = false;
      enable = true;
      extraCommands = ''
       iptables -A INPUT -p 47 -j ACCEPT
       iptables -A OUTPUT -p 47 -j ACCEPT
      ''; # 47 = GRE (for PPTP)
    };
  };

  nix = {
    binaryCaches = [ https://cache.nixos.org ];
    buildCores = 8;

    extraOptions = ''
      auto-optimise-store = true
      connect-timeout = 10
    '';

    gc = {
      automatic = true;
      dates = "18:00";
      options = "--delete-older-than 30";
    };

    maxJobs = 4;

    nrBuildUsers = 30;
    optimise = {
      automatic = true;
      dates = [ "18:00" ];
    };
    trustedBinaryCaches = [ https://cache.nixos.org ];
    useSandbox = true;
  };

  nixpkgs.config = {
    allowUnfree = true;

    firefox = {
      enableGoogleTalkPlugin = true;
      enableAdobeFlash = true;
    };
  };

  powerManagement = {
    enable = true;
    cpuFreqGovernor = "ondemand";
  };

  programs = {
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
      promptInit = ''
        autoload -U promptinit && promptinit && prompt clint
      '';
    };
  };

  security = {
    hideProcessInformation = true;

    sudo = {
        enable = true;
        wheelNeedsPassword = false;
    };
  };

  services = {
    locate.enable = true;

    nixosManual.enable = false;

    ntp = {
      enable = true;
      servers = [ "0.ca.pool.ntp.org" "1.ca.pool.ntp.org" "2.ca.pool.ntp.org" "3.ca.pool.ntp.org" ];
    };

    openssh = {
      enable = true;
      passwordAuthentication = false;
      permitRootLogin = "no";
    };

    upower.enable = true;

    xserver = {
      autorun = true;
      defaultDepth = 24;
      displayManager = {
        sessionCommands = with pkgs; lib.mkAfter ''
          ${coreutils}/bin/sleep 5 && ${parcellite}/bin/parcellite &
        '';
        slim = {
          enable = true;
        };
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
      windowManager.i3 = {
        enable = true;
      };
      xkbOptions = "ctrl:nocaps";
    };
  };

  system = {
    autoUpgrade = {
      channel = "https://nixos.org/channels/nixos-16.09-small";
      dates = "21:00";
      enable = false;
    };
    stateVersion = "16.09";
  };

  time = {
    timeZone = "America/Toronto";
  };

  users = {
    defaultUserShell = "${pkgs.zsh}/bin/zsh";

    extraUsers.nequi = {
     createHome = true;
     extraGroups = [ "docker" "wheel" ];
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

  virtualisation.docker = {
    enable = true;
    storageDriver = "btrfs";
  };
}
