{ config, pkgs, ... }:

{
  imports = [ /etc/nixos/hardware-configuration.nix ];

  boot.kernelPackages = pkgs.linuxPackages_4_2;

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  boot.kernel.sysctl = {
    "vm.swappiness" = 10;
  };

  swapDevices = [{device = "/dev/sda1";}];

  networking = {
    hostName = "nixie";
    extraHosts = "127.0.0.1 nixie";

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

  fonts = {
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

  time.timeZone = "UTC";

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
#    docker
    dropbox
#    ghc
    gitFull
#    gnupg
    htop
#    mariadb
    maven
    jdk
    open-vm-tools
    openssl
#    rpm
    sbt
    scala
    sublime3
    subversion
#    tomcat7
    wget
  ];

  programs.zsh = {
    enable = true;
    promptInit = ''
      autoload -U promptinit && promptinit && prompt clint
    '';
  };

  services = {
    xserver = {
      enable = true;
      defaultDepth = 24;
      videoDriver = "vmware";
      exportConfiguration = true;
      autorun = true;
      monitorSection = ''ModeLine "1920x1080" 146.00 1920 2048 2248 2576 1080 1083 1088 1120 -hsync +vsync'';
      resolutions = [{x = 1920; y = 1080;}];
      windowManager.awesome.enable = true;
      desktopManager.xterm.enable = false;
      displayManager.slim.enable = true;
    };

#    mysql = {
#      enable = true;
#      package = pkgs.mariadb;
#    };

#    tomcat = {
#      enable = true;
#      user = "itactics";
#    };

    openssh = {
      enable = true;
      passwordAuthentication = false;
      permitRootLogin = "no";
    };

    nscd.enable = false;
  };

  users.defaultUserShell = "/run/current-system/sw/bin/zsh";

  users.extraUsers.nequi = {
    name = "nequi";
    group = "users";
    uid = 1000;
    extraGroups = ["wheel"];
    createHome = true;
    home = "/home/nequi";
    shell = "/run/current-system/sw/bin/zsh";
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGHGhXTBqe9tG3bQaL1q1hpHr5q+YC/vKcCvNW2EVZN6 tsteinbach@CAM-EN-RFDV7N.local"
    ];
  };

  system.stateVersion = "15.09";

}
