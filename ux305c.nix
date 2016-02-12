{ config, pkgs, ... }:

{
  imports = [ /etc/nixos/hardware-configuration.nix ./ohmyzsh.nix ./xterm.nix ];

  boot = {
    cleanTmpDir = true;

    initrd.kernelModules = ["ahci" "aesni-intel"];

    kernel.sysctl = {
      "vm.swappiness" = 10;
    };

    #kernelPackages = pkgs.linuxPackages_latest;
    kernelPackages = pkgs.linuxPackages_testing;

    loader = {
      efi.canTouchEfiVariables = true;
      gummiboot.enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    atom
    binutils
    chromium
    dropbox
    encfs
    gitFull
    gradle
    htop
    iotop
    kotlin
    jdk
    maven
    #ncurses
    parcellite
    sbt
    scala
    texLiveFull
    upower
    xclip
    xtrlock-pam
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
    pulseaudio.enable = true;
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
      enable = true;
      allowedTCPPorts = [ 22 ];
    };

    wireless = {
      enable = true;
      networks.Germadian.pskRaw = "36c8af10dfa1bc96ca74afe305d0c4750ef5259d87d39e9051c1ab441f38dbd9";
      networks.SecurityBeginsWithTrust.pskRaw = "b5da43c13e23e1d6a8328ccd5128ac6b8b5add7852489d1da150535e13afad3b";
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

    upower.enable = true;

    xserver = {
      autorun = true;
      defaultDepth = 24;
      desktopManager.xterm.enable = false;
      displayManager = {
        sessionCommands = with pkgs; lib.mkAfter ''
          ${coreutils}/bin/sleep 30 && ${dropbox}/bin/dropbox &
          ${coreutils}/bin/sleep 5 && ${parcellite}/bin/parcellite &
        '';
        slim.enable = true;
      };
      enable = true;
      exportConfiguration = true;
      resolutions = [{x = 1920; y = 1080;} {x = 1280; y = 800;} {x = 1024; y = 768;}];
      synaptics.enable = true;
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
     #extraGroups = [ "wheel" ];
     group = "users";
     home = "/home/nequi";
     name = "nequi";
     shell = "${pkgs.zsh}/bin/zsh";
     uid = 1000;

     openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA7Jdj3a0bXoMTTE7dTLtAuB3aY5ZCTvWGhmlYYYFC/D timsteinbach@iPixel.local"
     ];
    };
  };
}
