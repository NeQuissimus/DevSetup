{ config, lib, pkgs, ... }:

{
  boot = {
    enableContainers = false;

    kernel.sysctl = {
      "vm.dirty_background_ratio" = 2; # Write back to disk at %
      "vm.dirty_ratio" = 3; # Write back to disk at %
      "vm.drop_caches" = 1; # Drop caches early
      "vm.swappiness" = 1; # Minimum swap usage
      "vm.vfs_cache_pressure" = 60; # Less reclaim pressure
    };

    loader = {
      efi.canTouchEfiVariables = true;

      grub.splashImage = "${pkgs.nixos-artwork.wallpapers.stripes-logo}/share/artwork/gnome/nix-wallpaper-stripes-logo.png";
    };

    plymouth = {
      enable = true;
      logo = "${pkgs.nixos-artwork.wallpapers.simple-light-gray}/share/artwork/gnome/nix-wallpaper-simple-light-gray.png";
      theme = "spinfinity";
    };
  };

  documentation.nixos.enable = false;

  environment = {
    etc."dnsmasq.hosts".text = (lib.fileContents ./etc/hosts);

    sessionVariables = {
      TERMINFO_DIRS = "/run/current-system/sw/share/terminfo";
    };

    systemPackages = with pkgs; [
      # Basics
      alsaUtils
      autocutsel
      bat
      binutils
      conky
      dnsutils
      exa
      feh
      firefox
      gitFull
      gnupg1compat
      htop
      i3lock-fancy
      jq
      oh-my-zsh
      ripgrep
      rofi
      rxvt_unicode-with-plugins
      skopeo

      ((pkgs.callPackage ./nixpkgs/nix-home.nix) { })
    ];
  };

  fonts = {
    enableFontDir = true;

    fonts = with pkgs; [
      dejavu_fonts
      emacs-all-the-icons-fonts
      font-awesome_4
      hasklig
      powerline-fonts
    ];

    fontconfig.defaultFonts.monospace = [ "DejaVu Sans Mono for Powerline" ];
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    pulseaudio.enable = true;
  };

  i18n = {
    consoleKeyMap = "us";
    defaultLocale = "en_CA.UTF-8";
  };

  networking = {
    nameservers = [ "127.0.0.1" ];
    timeServers = [ "0.ca.pool.ntp.org" "1.ca.pool.ntp.org" "2.ca.pool.ntp.org" "3.ca.pool.ntp.org" ];
  };

  nix = {
    autoOptimiseStore = true;
    binaryCaches = [ https://cache.nixos.org ];

    binaryCachePublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
    ];

    buildCores = 4;

    extraOptions = ''
      binary-caches-parallel-connections = 20
      connect-timeout = 10
    '';

    gc = {
      automatic = true;
      dates = "13:00";
      options = "--delete-older-than 60";
    };

    maxJobs = 8;
    nrBuildUsers = 30;

    optimise = {
      automatic = true;
      dates = [ "14:00" ];
    };
  };

  nixpkgs.config = {
    allowUnfree = true;
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
          hostNames = [ "github.com" "192.30.252.*" "192.30.253.*" "192.30.254.*" "192.30.255.*" ];
          publicKey = "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==";
        }
      ];

      startAgent = true;
    };

    zsh = {
      autosuggestions.enable = true;
      enable = true;
      enableCompletion = true;

      ohMyZsh = {
        customPkgs = [ pkgs.spaceship-prompt ];
        enable = true;
      };

      syntaxHighlighting = {
        enable = true;

        highlighters = [ "main" "brackets" "pattern" "root" "line"];
      };
    };
  };

  services = {
    dnsmasq = {
      alwaysKeepRunning = true;
      enable = true;

      extraConfig = ''
        no-hosts
        addn-hosts=/etc/dnsmasq.hosts
        strict-order
        cache-size=2000
        local-ttl=3600
        min-cache-ttl=3600

        conf-file=/etc/dnsmasq-conf.conf

        address=/bmcm-security-esentire.net/0.0.0.0
        address=/cowrk.me/0.0.0.0
        address=/esentife.com/0.0.0.0
        address=/esentiire.com/0.0.0.0
        address=/esentire-email-security.com/0.0.0.0
        address=/esentire.cam/0.0.0.0
        address=/esentireapis.com/0.0.0.0
        address=/esentiredns.com/0.0.0.0
        address=/esentrie.com/0.0.0.0
        address=/eshoney.com/0.0.0.0
        address=/getlink.nl/0.0.0.0
        address=/philezone.com/0.0.0.0
        address=/threatlab.io/0.0.0.0
      '';

      resolveLocalQueries = false;
    };

    nscd.enable = false;

    ntp = {
      enable = true;
      servers = [ "0.ca.pool.ntp.org" "1.ca.pool.ntp.org" "2.ca.pool.ntp.org" "3.ca.pool.ntp.org" ];
    };

    openssh.enable = true;

    redshift = {
      enable = false;
      latitude = "43.18";
      longitude = "-80.38";
      temperature.night = 1900;
    };

    tlp.enable = true;
    upower.enable = true;
    urxvtd.enable = true;

    xserver.displayManager.sessionCommands = with pkgs; lib.mkAfter ''
      ${feh}/bin/feh --bg-scale "${nixos-artwork.wallpapers.simple-dark-gray}/share/artwork/gnome/nix-wallpaper-simple-dark-gray.png" &
      ${xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr &
      ${autocutsel}/bin/autocutsel &
      ${autocutsel}/bin/autocutsel -s PRIMARY &
    '';
  };

  system.stateVersion = "18.09";

  time = {
    timeZone = "America/Toronto";
  };

  users = {
    defaultUserShell = "${pkgs.zsh}/bin/zsh";

    extraUsers.nequi = {
      extraGroups = [ "docker" "wheel" ];
      isNormalUser = true;
      name = "nequi";
      uid = 1000;
      openssh.authorizedKeys.keyFiles = [
        (builtins.fetchurl {
          url = "https://github.com/NeQuissimus.keys";
          sha256 = "14mg3ra36nxddqljb5fz360i7f1a1g1w3fv9wqb20fdgkk5rc2l5";
        })
      ];
    };

    users.docker = {
      subUidRanges = [ { startUid = 100000; count = 65536; } ];
      subGidRanges = [ { startGid = 100000; count = 65536; } ];
    };
  };

  virtualisation = {
    docker = {
      enable = true;
    };
  };
}
