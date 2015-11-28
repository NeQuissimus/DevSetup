{ config, pkgs, ... }:

{
  imports = [ /etc/nixos/hardware-configuration.nix ];

  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  swapDevices = [ { device = "/dev/sda1"; } ];

  boot.kernel.sysctl = {
    "vm.swappiness" = 10;
  };

  networking = {
    hostName = "nixPixel";
    extraHosts = "127.0.0.1 nixPixel";
    hostId = "6377bdf4";

    firewall = {
      enable = true;
      allowedTCPPorts = [ 22 ];
    };
  };

  nix = {
    gc = {
      automatic = true;
      dates = "10:00";
      options = "--delete-older-than 14";
    };

    extraOptions = ''
      auto-optimise-store = true
    '';

    package = pkgs.nixUnstable;

    useChroot = true;
  };

  powerManagement = {
    enable = true;
    cpuFreqGovernor = "ondemand";
  };
  
  i18n = {
    consoleFont = "source-code-pro";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time = {
    timeZone = "UTC";
  };

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      source-code-pro
    ];
  };

  nixpkgs.config = {
    allowUnfree = true;
    chromium = {
      enablePepperFlash = true;
      enablePepperPDF = true;
    };
  };

  environment.systemPackages = with pkgs; [
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
    texLiveFull
  ];

  programs.zsh = {
    enable = true;
    promptInit = ''
      autoload -U promptinit && promptinit && prompt clint
    '';
  };

  services = {
    openssh = {
      enable = true;
      passwordAuthentication = false;
      permitRootLogin = "no";
    };

    xserver = {
      enable = true;
      defaultDepth = 24;
      videoDriver = "intel";
      exportConfiguration = true;
      autorun = true;
      resolutions = [{x = 1280; y = 800;} {x = 1024; y = 768;}];
      windowManager.awesome.enable = true;
      desktopManager.xterm.enable = false;
      displayManager.slim.enable = true;
    };
  };

  users.defaultUserShell = "/run/current-system/sw/bin/zsh";

  users.extraUsers.nequi = {
   name = "nequi";
   group = "users";
   uid = 1000;
   extraGroups = [ "wheel" ];
   createHome = true;
   home = "/home/nequi";
   shell = "/run/current-system/sw/bin/zsh";
   openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC3W3w6Zf5ceFx+7FFfLKZZb2eeHf6fFETSfbBw954aiFQkVdo0MBgxICc8HzjSk+XKsrgYB/uDFs5e+qjyqmS2nKJMiMTkTRxG+HOiPYf50LI5eZPqB5mMORDOQZYShAYfMs5+dLfDT2K88zuAb4KoxoJVo2a6Q1x+NxJT6YKtcTgxmAGylqRJThvpVd/yoYoBKzlmWPI37iyeg+8GT5Qo9edzqYFIg4tN9lK1mzpZ+ybpL674+WM6X9bGdlKNn0JHi9BCsFUGYFvB9qpQuEZ8MRL/cvfDrERZuc+w6WaCcZobAd0s6GTylUjGGTSvrcXYxPexFMPZIgNe0qRjacfd tsteinbach@pixelbox.local"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFnJWxz0uwcdMkyjx3+MxO9ZNlVPaCGsUolCi9GtRDJ4 nequi@nixPixel"
   ];
  };
}
