{ config, lib, pkgs, ... }:

{
  imports = [ ./t460s-hardware.nix ./xmonad-work.nix ./t460s-wifi.nix ];

  boot = {
    cleanTmpDir = true;

    initrd.kernelModules = ["ahci" "aesni-intel"];

    kernel.sysctl = {
      "vm.dirty_writeback_centisecs" = 1500;
      "vm.drop_caches" = 3;
      "vm.laptop_mode" = 5;
      "vm.swappiness" = 1;
    };

    kernelPackages = pkgs.linuxPackages_latest;

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
      binutils
      conky
      dmenu
      exfat-utils
      fuse_exfat
      gitFull
      htop
      i3lock-fancy
      parcellite
      rxvt_unicode-with-plugins
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
    hostName = "nixus";

    extraHosts = ''
      127.0.0.1 nixus
      0.0.0.0 ftp.au.debian.org

      10.1.110.57 registry.internal
      10.1.110.83 confluence.internal
      10.1.110.113 gems.internal
      10.1.110.167 jira.esentire.com
      10.1.110.208 jenkins.internal
      10.1.110.243 gerrit.internal
      10.1.114.20 exchange.esentire.com
      10.1.110.130 build-01.internal
    '' + (lib.fileContents ./hosts);

    firewall = {
      allowedTCPPorts = [ 22 ];
      allowPing = false;
      enable = true;
    };

    nameservers = [ "10.1.115.20" "8.8.8.8" "10.1.114.53" "10.3.114.53" "8.8.4.4" "64.6.64.6" "64.6.65.6" ];
  };

  nix = {
    binaryCaches = [ https://cache.nixos.org ];
    buildCores = 8;

    extraOptions = ''
      auto-optimise-store = true
      binary-caches-parallel-connections = 3
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
          hostNames = [ "gerrit.internal" "10.1.110.243" ];
          publicKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAAAgQCab+Q4qxGALu4lQvDn8JHezrhJdsZSlK6QRXs/ZL0HD1u/qnRdH/WALeKKoRWG4O4H9ACbGwWl8EbFonhrbfE+0QgxwkGwm8DBZwKJV5aALhXKCREV0Zm/OamVmRGqUHQ5JUItCmfKt7mAqw1KheEBxMy2Qj3W/joqTNqL9tPipw==";
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

    pki.certificates = [
      (lib.fileContents ./registry.crt)
    ];

    sudo = {
      enable = true;
      wheelNeedsPassword = true;
    };
  };

  services = {
    locate.enable = true;

    nscd = {
      config = ''
        server-user             nscd
        threads                 2
        paranoia                no
        debug-level             0

        enable-cache            passwd          yes
        positive-time-to-live   passwd          600
        negative-time-to-live   passwd          20
        suggested-size          passwd          211
        check-files             passwd          yes
        persistent              passwd          no
        shared                  passwd          yes

        enable-cache            group           yes
        positive-time-to-live   group           3600
        negative-time-to-live   group           60
        suggested-size          group           211
        check-files             group           yes
        persistent              group           no
        shared                  group           yes

        enable-cache            hosts           yes
        positive-time-to-live   hosts           3600
        negative-time-to-live   hosts           15
        suggested-size          hosts           211
        check-files             hosts           yes
        persistent              hosts           yes
        shared                  hosts           yes
      '';
      enable = true;
    };

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

    urxvtd.enable = true;

    xserver = {
      autorun = true;
      defaultDepth = 24;
      displayManager = {
        sessionCommands = with pkgs; lib.mkAfter ''
          ${xlibs.xrandr}/bin/xrandr --output DP2-3 --crtc 1 --auto --pos 0x0 --output DP2-2 --crtc 2 --primary --auto --pos 1920x0 --output eDP1 --auto --pos 3840x0 &
          ${xorg.xsetroot}/bin/xsetroot -solid "#222222" &
          ${xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr &
          ${coreutils}/bin/sleep 5 && ${parcellite}/bin/parcellite &
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
      xkbOptions = "ctrl:nocaps";
      xrandrHeads = [ "DP2-3" "DP2-2" "eDP1" ];
    };
  };

  system = {
    autoUpgrade = {
      channel = "https://nixos.org/channels/nixos-17.03";
      dates = "9:00";
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

  virtualisation = {
    docker = {
      enable = true;
      storageDriver = "btrfs";
    };

    virtualbox.host.enable = true;
  };
}
