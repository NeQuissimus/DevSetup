{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ./xterm.nix ./ohmyzsh.nix ];

  boot = {
    cleanTmpDir = true;

    initrd.checkJournalingFS = false;

    loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/sda";
    };

    kernel.sysctl = {
      "vm.swappiness" = 10;
    };

    kernelPackages = pkgs.linuxPackages_latest;
  };

  environment = {
    systemPackages = with pkgs; [
      binutils
      chromium
      dropbox
      gitFull
      htop
      maven
      jdk
      sbt
      scala
      sublime3
    ];
  };

  fonts = {
    enableCoreFonts = true;
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      source-code-pro
    ];
  };

  i18n = {
    consoleFont = "source-code-pro";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  networking = {
    hostName = "nixie";
    extraHosts = "127.0.0.1 nixie";

    firewall = {
      enable = true;
      allowedTCPPorts = [ 22 ];
    };
  };

  nix = {
    binaryCaches = [ http://cache.nixos.org http://hydra.nixos.org ];
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
    ssh = {
      agentTimeout = "2h";
    };

    zsh = {
      enable = true;
      promptInit = ''
        autoload -U promptinit && promptinit && prompt clint
      '';
    };
  };

  services = {
    xserver = {
      enable = true;
      defaultDepth = 24;
      videoDriver = "virtualbox";
      exportConfiguration = true;
      autorun = true;
      monitorSection = ''ModeLine "1920x1080" 146.00 1920 2048 2248 2576 1080 1083 1088 1120 -hsync +vsync'';
      resolutions = [ {x = 1920; y = 1080;} ];
      windowManager.awesome.enable = true;
      desktopManager.xterm.enable = false;
      displayManager.slim.enable = true;
    };

    openssh = {
      enable = true;
      passwordAuthentication = false;
      permitRootLogin = "no";
    };

    nscd.enable = false;
  };

  swapDevices = [ {device = "/dev/sda1";} ];

  system.stateVersion = "15.09";

  time.timeZone = "UTC";

  users = {
    defaultUserShell = "/run/current-system/sw/bin/zsh";

    extraUsers.nequi = {
      createHome = true;
      extraGroups = [ "wheel" ];
      group = "users";
      home = "/home/nequi";
      name = "nequi";
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA7Jdj3a0bXoMTTE7dTLtAuB3aY5ZCTvWGhmlYYYFC/D timsteinbach@iPixel.local"
      ];
      shell = "/run/current-system/sw/bin/zsh";
      uid = 1000;
    };
  };

  virtualisation.virtualbox.guest.enable = true;
}
