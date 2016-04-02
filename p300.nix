{ config, pkgs, ... }:

{
  imports = [ ./p300-hardware.nix ];

  boot = {
    cleanTmpDir = true;

    initrd.kernelModules = ["ahci" "aesni-intel"];

    kernel.sysctl = {
      "vm.drop_caches" = 3;
      "vm.swappiness" = 5;
    };

    kernelPackages = pkgs.linuxPackages_latest;

    loader = {
      gummiboot.enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    atom
    binutils
    chromium
    gitFull
    firefox
    htop
    iotop
    oh-my-zsh
    parcellite
    unzip
    upower
    xclip
    xtrlock-pam

    jdk
    maven

    slack
  ];

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      source-code-pro
    ];
    fontconfig.defaultFonts.monospace = [ "Source Code Pro" ];
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    pulseaudio.enable = false;
  };

  i18n = {
    consoleFont = "source-code-pro";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  networking = {
    hostName = "nixus";
    extraHosts = ''
      127.0.0.1 nixus
      127.0.0.1 ftp.au.debian.org
    ''; # Basically kill ftp.au.debian.org

    firewall = {
      allowedTCPPorts = [ 22 80 1723 8080 ];
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

    extraOptions = ''
      auto-optimise-store = true
    '';

    gc = {
      automatic = true;
      dates = "10:00";
      options = "--delete-older-than 14";
    };

    package = pkgs.nixUnstable;

    trustedBinaryCaches = [ https://cache.nixos.org https://hydra.nixos.org ];

    useChroot = true;
  };

  nixpkgs.config = {
    allowUnfree = true;
    chromium = {
      enablePepperFlash = true;
      enablePepperPDF = true;
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

  services = {
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
      desktopManager.xterm.enable = false;
      displayManager = {
        sessionCommands = with pkgs; lib.mkAfter ''
          ${coreutils}/bin/sleep 5 && ${parcellite}/bin/parcellite &
        '';
        slim.enable = true;
      };
      enable = true;
      exportConfiguration = true;
      resolutions = [{x = 1920; y = 1200;} {x = 1280; y = 800;}];
      videoDriver = "intel";
      windowManager.awesome = {
        enable = true;
        luaModules = [ pkgs.luaPackages.vicious ];
      };
      xrandrHeads = [ "DP1" "DP2" ];
    };
  };

  system = {
    autoUpgrade = {
      dates = "10:00";
      enable = true;
    };
    stateVersion = "16.03";
  };

  time = {
    timeZone = "America/Toronto";
  };

  users = {
    defaultUserShell = "${pkgs.zsh}/bin/zsh";

    extraUsers.nequi = {
     createHome = true;
     extraGroups = [ "wheel" ];
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
}
