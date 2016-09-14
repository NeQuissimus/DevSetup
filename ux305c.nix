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

    #kernelPackages = pkgs.linuxPackages_grsec_nixos;
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
    feh
    firefox
    gettext
    gitFull
    gnupg
    gnupg1compat
    htop
    i3lock-fancy
    iotop
    keybase-go
    oh-my-zsh
    pandoc
    parcellite
    unzip
    upower
    vlc
    xclip
    xtrlock-pam

    # VPN
    ppp
    pptp

    # Java
    gradle
    jdk
    maven

    # Scala
    scala
    sbt

    # Lagom
    activator

    # Custom
    (import nixpkgs/sublime3-dev.nix)

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
      '';
    };
  };

  nix = {
    binaryCaches = [ https://cache.nixos.org https://hydra.nixos.org ];
    binaryCachePublicKeys = [ "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs=" ];
    buildCores = 8;

    extraOptions = ''
      auto-optimise-store = true
      connect-timeout = 10
    '';

    gc = {
      automatic = true;
      dates = "10:00";
      options = "--delete-older-than 30";
    };

    maxJobs = 4;

    nrBuildUsers = 30;
    trustedBinaryCaches = [ https://cache.nixos.org https://hydra.nixos.org ];
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
    ssh.startAgent = true;
    zsh = {
      enable = true;
      promptInit = ''
        autoload -U promptinit && promptinit && prompt clint
      '';
    };
  };

  security = {
#    grsecurity.enable = true;

    hideProcessInformation = true;

    sudo = {
        enable = true;
        wheelNeedsPassword = false;
    };
  };

  services = {
    fail2ban = {
        enable = false;
        jails.ssh-iptables = ''
          enabled = true
        '';
    };

    locate.enable = true;

    nixosManual.enable = false;

    nscd.enable = false;

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
      channel = "https://nixos.org/channels/nixos-16.09";
      dates = "01:00";
      enable = true;
    };
    stateVersion = "16.09";
  };

  time = {
    timeZone = "UTC";
  };

  users = {
    defaultUserShell = "${pkgs.zsh}/bin/zsh";

    extraUsers.nequi = {
     createHome = true;
     extraGroups = [ "docker" "wheel" ];
     group = "users";
     home = "/home/nequi";
     name = "nequi";
     shell = "${pkgs.zsh}/bin/zsh";
     uid = 1000;

     openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA7Jdj3a0bXoMTTE7dTLtAuB3aY5ZCTvWGhmlYYYFC/D timsteinbach@iPixel.local"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBL3lMFtXtxowkw4tM2irAQbVODOyBomOYchi4ClTNxV nequi@nixus"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCyku7sT/Wa20VjAUoGwOpmCbl5v2omrX9mZLJLnfTuYLxzqNsIZha2N5DjipIzxhjh8XoJG+8n/L7hO9tgrst4mhJZdAOEntS+rr4HSHoojzmh6Vkc1A6j9aS+qNUy+ZZYIZ6ENeVge2J4EobMFnlLP0r31s7rmYnXX1hH7RvhVqUf0Tg6/J4rEHKbwPUXkpto4WIR14yKzBxoO2zWX4X61viMSGUAeNQ35dGVd7JFv+LbCJW7HcyehbfFCb2wOa+L1EEGVaLJccGhQzzFd6svlr1ECxcM2L11zZTVJ6lYHMYKGAHxcIPMwIkoOyKtCtRPL4giJ43C6OBkTZqk0jm3 nequi@nixus"
     ];
    };
  };

  virtualisation.docker = {
    enable = true;
    storageDriver = "btrfs";
  };
}
