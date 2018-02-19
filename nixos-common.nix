{ config, lib, pkgs, ... }:

{
  boot = {
    enableContainers = false;

    initrd = {
      postMountCommands = ''
        chmod 777 /etc/xmonad # Hack because xmonad needs to write into the folder
        chmod 777 /etc/.config # Hack, so that other things can write in here
      '';
    };

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

  environment = {
    etc.".config/.gitconfig".text = ''
      [push]
              default = upstream
              followTags = true
      [user]
              email = tim@nequissimus.com
              name = Tim Steinbach
              signingkey = 0588CEBD610D7123
      [alias]
              bclean = "!f() { git fetch --prune && git branch --merged ''${1-master} | grep -v " ''${1-master}$" | xargs -r git branch -d && git branch -r | awk '{print $1}' | egrep -v -f /dev/fd/0 <(git branch -vv | grep origin) | awk '{print $1}' | xargs -r git branch -d; }; f"
              clear = clean -xfd
              hard = reset --hard origin/master
              lastcommit = for-each-ref --sort=committerdate refs --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(contents:subject) - %ae (%(color:green)%(committerdate:relative)%(color:reset))'
              lg = log --all --decorate --color --graph --pretty=format:'%Cred%h%Creset %Cgreen(%cr)%Creset - %s %C(bold blue)<%an>[%G?]%Creset%C(auto)%d%Creset' --abbrev-commit
              ll = log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate --numstat
              pr = "!f() { git fetch origin pull/''${1}/head:pr-''${1}; git checkout pr-''${1}; }; f"
              prup = "!f() { git fetch upstream pull/''${1}/head:pr-''${1}; git checkout pr-''${1}; }; f"
              rename = "!f() { git push -u origin origin/"''${1}":refs/heads/"''${2}"; git push origin :"''${1}"; }; f"
              stable = "!f() { git checkout release-17.09  && git pull && git cherry-pick -x ''${1} && git push && git checkout master; }; f"
              t = tag -s -a
              undo = reset HEAD~1 --mixed
              up = !git pull --rebase --prune $@ && git submodule update --init --recursive
      [interactive]
              diffFilter = diff-highlight
      [diff]
              compactionHeuristic = true
      [remote "origin"]
              prune = true
      [branch]
              autosetuprebase = always
      [commit]
              gpgsign = true
      [tag]
              forceSignAnnotated = true
      [apply]
              whitespace = fix
    '';

    etc."dnsmasq.hosts".text = (lib.fileContents ./etc/hosts);

    sessionVariables = {
      TERMINFO_DIRS = "/run/current-system/sw/share/terminfo";
      XDG_CONFIG_HOME = "/etc/.config";
      XMONAD_CONFIG_DIR = "/etc/xmonad";
    };

    systemPackages = with pkgs; [
      # Basics
      autocutsel
      binutils
      conky
      exa
      firefox
      gitFull
      gnupg1compat
      htop
      i3lock-fancy
      jq
      oh-my-zsh
      rofi
      rxvt_unicode-with-plugins
      skopeo
      vscode
    ];
  };

  fonts = {
    enableFontDir = true;

    fonts = with pkgs; [
      dejavu_fonts
      font-awesome-ttf
    ];

    fontconfig.defaultFonts.monospace = [ "DejaVu Sans Mono" ];
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
      dates = "12:00";
      options = "--delete-older-than 60";
    };

    maxJobs = 8;
    nrBuildUsers = 30;
    package = pkgs.nixUnstable;
    useSandbox = true;
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
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;

      ohMyZsh = {
        enable = true;
      };

      promptInit = (lib.fileContents ./_home/zshrc);

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
        address=/esentire-email-security.com/0.0.0.0
        address=/esentireapis.com/0.0.0.0
        address=/esentiredns.com/0.0.0.0
        address=/eshoney.com/0.0.0.0
        address=/getlink.nl/0.0.0.0
        address=/philezone.com/0.0.0.0
        address=/threatlab.io/0.0.0.0
      '';

      resolveLocalQueries = false;
    };

    nscd.enable = false;
    nixosManual.enable = false;

    ntp = {
      enable = true;
      servers = [ "0.ca.pool.ntp.org" "1.ca.pool.ntp.org" "2.ca.pool.ntp.org" "3.ca.pool.ntp.org" ];
    };

    redshift = {
      enable = true;
      latitude = "43.18";
      longitude = "-80.38";
      temperature.night = 1900;
    };

    tlp.enable = true;
    upower.enable = true;
    urxvtd.enable = true;
  };

  system.stateVersion = "17.09";

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
