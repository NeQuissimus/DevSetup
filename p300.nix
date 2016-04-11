{ config, pkgs, ... }:

{
  imports = [ ./p300-hardware.nix ];

  boot = {
    cleanTmpDir = true;

    initrd.kernelModules = ["ahci" "aesni-intel"];

    kernel.sysctl = {
      "vm.drop_caches" = 3;
      "vm.swappiness" = 5;
    };

    kernelPackages = pkgs.linuxPackages_latest;

    loader = {
      gummiboot.enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    atom
    binutils
    chromium
    gitFull
    firefox
    htop
    iotop
    oh-my-zsh
    parcellite
    tcpdump
    unzip
    xclip
    xtrlock-pam

    jdk
    maven
    scala

    slack

    python
    mysql

    liquibase
    nodejs
    openssl
    postgresql_jdbc
    protobuf2_5
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
      allowedTCPPorts = [ 22 80 1723 8080 ];
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
    enable = false;
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
    hideProcessInformation = true;
  };

  services = {
    nginx = {
      httpConfig = ''
server {
    listen 80;
    server_name $HOSTNAME;
    port_in_redirect off;
    #charset koi8-r;
    access_log /var/log/nginx/log/host.access.log combined;
    location / {
        set $should_proxy "";
        set $upgrade_header "";

        if ($http_sec_jbossremoting_key) {
            set $should_proxy "Y";
        }

        if ($http_sec_hornetqremoting_key) {
            set $should_proxy "Y";
        }

        if ($should_proxy = Y) {
            proxy_pass http://127.0.0.1:8080;
            set $upgrade_header "upgrade";
        }

        proxy_buffering off;
        proxy_read_timeout 120s;
        proxy_http_version 1.1;
        proxy_set_header sec_jbossremoting_key $http_sec_jbossremoting_key;
        proxy_set_header sec_hornetqremoting_key $http_sec_hornetqremoting_key;
        proxy_set_header upgrade $http_upgrade;
        proxy_set_header connection $upgrade_header;
        proxy_set_header host $http_host;

        add_header X-UA-Compatible IE=edge;
        root /home/nequi/dev/xms/cms/ui/web;
        index index.html;
    }
    location /cms-test {
        alias /home/nequi/dev/xms/cms/ui/test/mocha;
    }
    location /api {
        proxy_pass http://127.0.0.1:8080/sdp-web/api;
    }
    location /SDP-war {
        proxy_pass http://127.0.0.1:8080/SDP-war;
    }
    location /integration {
        proxy_pass http://127.0.0.1:8080/integration-test-support/integration;
    }
    location /diagnostics {
        alias /var/diagnostics;
        autoindex on;
    }
}
server {
    listen 9080;
    server_name $HOSTNAME;
    port_in_redirect off;
    #charset koi8-r;
    #access_log /var/log/nginx/log/host.access.log main;
    location / {
        add_header X-UA-Compatible IE=edge;
        root /home/nequi/dev/xms/cms/ui/build/release;
        index index.html;
    }
    location /api {
        proxy_pass http://127.0.0.1:8080/sdp-web/api;
    }
    location /SDP-war {
        proxy_pass http://127.0.0.1:8080/SDP-war;
    }
    location /integration {
        proxy_pass http://127.0.0.1:8080/integration-test-support/integration;
    }
    location /diagnostics {
        alias /var/diagnostics;
        autoindex on;
    }
}
server {
    listen 443 ssl;
    server_name $HOSTNAME;
    port_in_redirect off;
    ssl_certificate /usr/local/cms/sslCert/devCert.crt;
    ssl_certificate_key /usr/local/cms/sslCert/devCert.key;
    ssl_session_cache shared:SSL:1m;
    ssl_session_timeout 5m;
    ssl_ciphers HIGH:!aNULL:!MD5;
    ssl_prefer_server_ciphers on;
    location / {
        add_header X-UA-Compatible IE=edge;
        root /home/nequi/dev/xms/cms/ui/web;
        index index.html;
    }
    location /api {
        proxy_pass http://127.0.0.1:8080/sdp-web/api;
    }
    location /SDP-war {
        proxy_pass http://127.0.0.1:8080/SDP-war;
    }
    location /integration {
        proxy_pass http://127.0.0.1:8080/integration-test-support/integration;
    }
    location /diagnostics {
        alias /var/diagnostics;
        autoindex on;
    }
}
      '';
      enable = true;
    };

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

    postgresql = {
      dataDir = "/var/db/postgres93";
      enable = true;
      initialScript = ./emc_init.sql;
      package = pkgs.postgresql93;
    };

    upower.enable = true;

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
      windowManager.awesome = {
        enable = true;
        luaModules = [ pkgs.luaPackages.vicious ];
      };
      xrandrHeads = [ "DP1" "DP2" ];
    };
  };

  system = {
    autoUpgrade = {
      dates = "10:00";
      enable = true;
    };
    stateVersion = "16.03";
  };

  time = {
    timeZone = "America/Toronto";
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
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA7Jdj3a0bXoMTTE7dTLtAuB3aY5ZCTvWGhmlYYYFC/D timsteinbach@iPixel.local"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBL3lMFtXtxowkw4tM2irAQbVODOyBomOYchi4ClTNxV nequi@nixus"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCyku7sT/Wa20VjAUoGwOpmCbl5v2omrX9mZLJLnfTuYLxzqNsIZha2N5DjipIzxhjh8XoJG+8n/L7hO9tgrst4mhJZdAOEntS+rr4HSHoojzmh6Vkc1A6j9aS+qNUy+ZZYIZ6ENeVge2J4EobMFnlLP0r31s7rmYnXX1hH7RvhVqUf0Tg6/J4rEHKbwPUXkpto4WIR14yKzBxoO2zWX4X61viMSGUAeNQ35dGVd7JFv+LbCJW7HcyehbfFCb2wOa+L1EEGVaLJccGhQzzFd6svlr1ECxcM2L11zZTVJ6lYHMYKGAHxcIPMwIkoOyKtCtRPL4giJ43C6OBkTZqk0jm3 nequi@nixus"
     ];
    };
  };
}
