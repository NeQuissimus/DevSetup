{ config, lib, pkgs, ... }:

let
  ad=lib.fileContents ./etc/ad;
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

  networking.firewall.allowedTCPPorts = [ 631 ]; # CUPS

  networking.hostName = "nixus-desktop";

  networking.hosts = {
    "127.0.0.1" = ["${config.networking.hostName}" "localhost"];
    "10.1.108.118" = ["github.internal" "github.esentire.com"];
    "10.1.108.152" = ["jen001p"];
    "10.1.108.153" = ["jen002p"];
    "10.1.108.154" = ["jen003" "registry.internal" "portus-1.internal"];
    "10.1.108.160" = ["jen004p"];
    "10.1.110.54" = ["artifactory.internal"];
    "10.1.110.113" = ["gems.internal"];
    "10.1.110.130" = ["build-01.internal"];
    "10.1.110.167" = ["jira.esentire.com" "jira.internal"];
    "10.1.110.178" = ["secnet.esentire.com"];
    "10.1.110.191" = ["hipchat.esentire.com"];
    "10.1.110.208" = ["jenkins.internal"];
    "10.1.110.243" = ["gerrit.internal"];
    "10.1.110.83" = ["confluence.internal" "confluence.esentire.com"];
    "10.1.114.20" = ["exchange.esentire.com"];
    "10.1.204.234" = ["soc.uat"];
    "10.203.0.31" = ["kubeapi.qa"];
    "10.203.1.101" = ["kubeapi.dev" "kubeapi.test"];
    #"10.203.1.106" = ["kubeapi.dev"];
    #"10.203.1.107" = ["kubeapi.dev"];
    "172.16.0.254" = ["captiveportal-login.esentire.com" "wifi.esentire.com"];

    "0.0.0.0" = [ "ftp.au.debian.org" ];
  };

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
