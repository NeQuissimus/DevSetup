{ config, pkgs, ... }:
{
  imports = [ ./hardware-configuration.nix ];

  boot = {
    cleanTmpDir = true;

    loader = {
      grub = {
        device = "/dev/sda";
        enable = true;
        version = 2;
      };
    };

    kernel = {
      sysctl = {
        "vm.drop_caches" = 1;
        "vm.laptop_mode" = 5;
        "vm.swappiness" = 1;
      };
    };

    kernelPackages = pkgs.linuxPackages_grsec_nixos;
  };

  environment = {
    systemPackages = with pkgs; [
      openjdk8
    ];
  };

  i18n = {
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  networking = {
    firewall = {
      allowPing = false;
      allowedTCPPorts = [ 22 ];
      enable = true;
    };

    hostName = "minecraft";
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

    optimise = {
      automatic = true;
      dates = [ "18:00" ];
    };

    trustedBinaryCaches = [ https://cache.nixos.org ];
    useSandbox = true;
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
    };
  };

  powerManagement = {
    enable = true;
    cpuFreqGovernor = "ondemand";
  };

  security = {
    grsecurity.enable = true;
    hideProcessInformation = true;
  };

  services = {
    locate = {
      enable = true;
    };

    minecraft-server = {
      enable = true;
      jvmOpts = "-Xms1024M -Xmx3072M";
      openFirewall = true;
    };

    nixosManual = {
      enable = false;
    };

    ntp = {
      enable = true;
      servers = [ "0.ca.pool.ntp.org" "1.ca.pool.ntp.org" "2.ca.pool.ntp.org" "3.ca.pool.ntp.org" ];
    };

    openssh = {
      enable = true;
      passwordAuthentication = false;
      permitRootLogin = "no";
    };

    upower = {
      enable = true;
    };
  };

  system = {
    autoUpgrade = {
      channel = "https://nixos.org/channels/nixos-unstable";
      dates = "19:00";
      enable = true;
    };

    stateVersion = "17.03";
  };

  time = {
    timeZone = "America/Toronto";
  };

  users = {
    extraUsers = {
      nequi = {
        createHome = true;
        group = "users";
        home = "/home/nequi";
        name = "nequi";

        openssh = {
          authorizedKeys = {
            keys = [
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO3R/fRHsHFFND32a0TuT5sqsM0y9Y6YENy03CndJoWz nequi@nixus"
            ];
          };
        };

        useDefaultShell = true;
        uid = 1000;
      };
    };
  };
}
