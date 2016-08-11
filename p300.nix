{ config, pkgs, ... }:

{
  imports = [ ./p300-hardware.nix ];

  boot = {
    cleanTmpDir = true;

    initrd.kernelModules = ["ahci" "aesni-intel"];

    kernel.sysctl = {
      "vm.drop_caches" = 1;
      "vm.swappiness" = 5;
    };

#    kernelPackages = pkgs.linuxPackages_latest;
#    kernelPackages = pkgs.linuxPackages;
    kernelPackages = pkgs.linuxPackages_grsec_nixos;

    loader = {
      systemd-boot.enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    binutils
    chromium
    emacs
    gitFull
    firefox
    gnupg
    gnupg1compat
    google-chrome
    htop
    i3lock-fancy
    keybase
    iotop
    oh-my-zsh
    parcellite
    sublime3
    tcpdump
    unzip
    xclip
    xtrlock-pam

    idea.idea-community
    jdk
#    (maven.override { jdk = openjdk7; })
    maven
    scala
    sbt

    pandoc

    ( lib.overrideDerivation slack (attrs: {
      name = "slack-2.0.3";
        src = fetchurl {
        url = "https://slack-ssb-updates.global.ssl.fastly.net/linux_releases/slack-desktop-2.0.3-amd64.deb";
        sha256 = "0pp8n1w9kmh3pph5kc6akdswl3z2lqwryjg9d267wgj62mslr3cg";
        name = "slack-desktop-2.0.3-amd64.deb";
      };
    }))

    python
    mysql
    postgresql

    # QX Cloud
    protobuf3_0

    # QX Boost
    ## UI
    gcc
    gnumake
    nodejs
    nodePackages.gulp
    ## Data Generator
    python3
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
    pulseaudio.enable = true;
  };

  i18n = {
#    consoleFont = "source-code-pro";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  networking = {
    hostName = "nixus";
    extraHosts = ''
      127.0.0.1 nixus
      127.0.0.1 ftp.au.debian.org
      52.6.31.232 github
    ''; # Basically kill ftp.au.debian.org

    firewall = {
      allowedTCPPorts = [ 22 80 443 1723 5432 8080 9990 ];
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

    buildCores = 0;

    extraOptions = ''
      auto-optimise-store = true
    '';

    gc = {
      automatic = true;
      dates = "10:00";
      options = "--delete-older-than 30";
    };

    trustedBinaryCaches = [ https://cache.nixos.org https://hydra.nixos.org ];

    useSandbox = true;
  };

  nixpkgs.config = {
    allowUnfree = true;
    
    chromium = {
      enablePepperFlash = true;
      enablePepperPDF = true;
    };

    firefox = {
      enableGoogleTalkPlugin = true;
      enableAdobeFlash = true;
    };
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
    chromiumSuidSandbox.enable = true;
    grsecurity.enable = true;
    sudo.wheelNeedsPassword = false;
  };

  services = {
    klogd.enable = false;

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

    xserver = {
      autorun = true;
      defaultDepth = 24;
      desktopManager.xterm.enable = false;
      displayManager = {
        sessionCommands = with pkgs; lib.mkAfter ''
          ${coreutils}/bin/sleep 5 && ${parcellite}/bin/parcellite &
        '';
        slim = {
          enable = true;
          extraConfig = ''
            numlock on
          '';
        };
      };
      enable = true;
      exportConfiguration = true;
      resolutions = [{x = 1920; y = 1200;} {x = 1280; y = 800;}];
      videoDriver = "intel";
      windowManager.i3 = {
        enable = true;
      };
      xrandrHeads = [ "DP1" "DP2" ];
    };
  };

  system = {
    stateVersion = "16.09";
  };

  time = {
    timeZone = "America/Toronto";
  };

  users = {
    defaultUserShell = "${pkgs.zsh}/bin/zsh";

    groups.vboxusers.members = [ "nequi" ];

    users.nequi = {
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

  virtualisation = {
    docker = {
      enable = true;
      extraOptions = "--insecure-registry docker-mama.lab.exinda.com";
    };

    virtualbox.host = {
      enable = true;
    };
  };
}
