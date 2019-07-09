{ config, lib, pkgs, ... }:

let
  ad = lib.fileContents ./etc/ad;
  esentire-dns = import ./esentire-dns.nix;
in {
  imports = [ ./nixos-common.nix ./nixos-harden.nix ./nixos-xmonad.nix ./p320-hardware.nix ];

  boot.loader.systemd-boot.enable = true;

  boot.kernel.sysctl = {
    "vm.max_map_count" = 262144; # Increase map count for ElasticSearch
  };

  environment.etc."ethernet.pem".text = (lib.fileContents ./etc/ethernet.pem);

  environment.systemPackages = with pkgs; [
    awscli
    gitter
    kubectl
    scala
    slack-dark
  ];

  environment.variables.QTWEBENGINE_DISABLE_SANDBOX = "1";

  networking.firewall.allowedTCPPorts = [ 22 53 631 ];

  networking.hostName = "nixus-desktop";

  networking.hosts = {
    "127.0.0.1" = ["${config.networking.hostName}" "localhost"];
    "0.0.0.0" = [ "ftp.au.debian.org" ];
  } // esentire-dns.hosts;

  networking.networkmanager = {
    enable = true;

    extraConfig = ''
      [connection]
      id=eSentire Office
      uuid=1ad250e1-a36c-4729-8062-2c4436e8beb5
      type=ethernet
      permissions=

      [802-1x]
      ca-cert=/etc/ethernet.pem
      eap=peap;
      identity=ESENTIRE\\tsteinbach
      password=${ad}
      phase2-auth=mschapv2
    '';

    insertNameservers = [ "127.0.0.1#53" ];
  };

  nixpkgs.config = {
    firefox.enableAdobeFlash = true;
    firefox.enableAdobeFlashDRM = true;
  };

  services.avahi.enable = true;
  services.avahi.nssmdns = true;

  # List will be flipped
  services.dnsmasq.servers = [
    "64.6.64.6" # Verisign
    "64.6.65.6" # Verisign
    "192.168.114.199" # Internal colo
    "10.1.114.53" # Internal
    "10.3.114.53" # Internal
    "10.3.115.20" # Internal
    "10.1.120.31" # Internal
    "1.1.1.1" # CloudFlare
    "9.9.9.9" # Quad 9
    "10.1.115.20" # Internal
  ];

  services.emacs = {
    enable = true;
    package = import ./nixpkgs/emacs.nix { pkgs = pkgs; };
  };

  services.nscd.enable = true;

  services.openssh = {
    enable = true;
  };

  security.pki.certificates = [
    (lib.fileContents ./registry.crt)
    (lib.fileContents ./ca.internal.crt.pem)
  ];

  services.printing = {
    enable = true;
  };

  services.xserver.displayManager.sessionCommands = with pkgs; lib.mkAfter ''
    sleep 3 && ${xlibs.xrandr}/bin/xrandr --output HDMI2 --primary --crtc 2 --auto --pos 0x500 --output DP1 --crtc 1 --auto --rotate left --pos 1920x0 --output HDMI1 --crtc 1 --auto --pos 3000x0 --rotate left &
    ${feh}/bin/feh --bg-scale "${nixos-artwork.wallpapers.simple-dark-gray}/share/artwork/gnome/nix-wallpaper-simple-dark-gray.png" &
    ${xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr &
    ${autocutsel}/bin/autocutsel &
    ${autocutsel}/bin/autocutsel -s PRIMARY &
  '';

  services.xserver.videoDriver = "intel";

  services.xserver.xrandrHeads = [
    "HDMI1" { monitorConfig = ''Option "Rotate" "left"''; output = "HDMI1"; }
    "DP1" { output = "DP1"; primary = true; }
    "HDMI2" { monitorConfig = ''Option "Rotate" "left"''; output = "HDMI2"; }
  ];

  system.autoUpgrade = {
    channel = "https://nixos.org/channels/nixos-unstable";
    enable = true;
  };
}
