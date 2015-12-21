{ config, pkgs, ... }:

{
  imports = [ /etc/nixos/hardware-configuration.nix ./ohmyzsh.nix ./xterm.nix ];

  boot = {
    cleanTmpDir = true;

    kernel.sysctl = {
      "vm.swappiness" = 10;
    };

    kernelPackages = pkgs.linuxPackages_latest;

    loader.grub = {
      enable = true;
      version = 2;
      device= "/dev/sda";
    };
  };

  environment.systemPackages = with pkgs; [
    binutils
    chromium
    dropbox
    gitFull
    gradle
    htop
    iotop
    jre
    maven
    sublime3
    texLiveFull
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
  };

  i18n = {
    consoleFont = "source-code-pro";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  networking = {
    hostName = "nixel";
    extraHosts = ''
      127.0.0.1 nixel
      127.0.0.1 ftp.au.debian.org
    ''; # Basically kill ftp.au.debian.org

    firewall = {
      enable = true;
      allowedTCPPorts = [ 22 ];
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
          ${coreutils}/bin/sleep 30 && ${dropbox}/bin/dropbox &
        '';
        slim.enable = true;
      };
      enable = true;
      exportConfiguration = true;
      resolutions = [{x = 1280; y = 800;} {x = 1024; y = 768;}];
      videoDriver = "intel";
      windowManager.awesome.enable = true;
    };
  };

  time = {
    timeZone = "UTC";
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
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC3W3w6Zf5ceFx+7FFfLKZZb2eeHf6fFETSfbBw954aiFQkVdo0MBgxICc8HzjSk+XKsrgYB/uDFs5e+qjyqmS2nKJMiMTkTRxG+HOiPYf50LI5eZPqB5mMORDOQZYShAYfMs5+dLfDT2K88zuAb4KoxoJVo2a6Q1x+NxJT6YKtcTgxmAGylqRJThvpVd/yoYoBKzlmWPI37iyeg+8GT5Qo9edzqYFIg4tN9lK1mzpZ+ybpL674+WM6X9bGdlKNn0JHi9BCsFUGYFvB9qpQuEZ8MRL/cvfDrERZuc+w6WaCcZobAd0s6GTylUjGGTSvrcXYxPexFMPZIgNe0qRjacfd tsteinbach@pixelbox.local"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFnJWxz0uwcdMkyjx3+MxO9ZNlVPaCGsUolCi9GtRDJ4 nequi@nixPixel"
     ];
    };
  };
}
