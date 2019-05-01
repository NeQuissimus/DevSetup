{ config, lib, pkgs, ... }:

{
  imports = [ ./nixos-common.nix ./nixos-harden.nix ./nixos-xmonad.nix ./p320-hardware.nix ];

  boot.loader.systemd-boot.enable = true;

  boot.kernel.sysctl = {
    "vm.max_map_count" = 262144; # Increase map count for ElasticSearch
  };

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
    sleep 3 && ${xlibs.xrandr}/bin/xrandr --output DP1 --crtc 1 --primary --auto --pos 0x0 --output HDMI2 --crtc 2 --rotate left --auto --pos 1920x0
    ${feh}/bin/feh --bg-scale "${nixos-artwork.wallpapers.simple-dark-gray}/share/artwork/gnome/nix-wallpaper-simple-dark-gray.png" &
    ${xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr &
    ${autocutsel}/bin/autocutsel &
    ${autocutsel}/bin/autocutsel -s PRIMARY &
  '';

  services.xserver.videoDriver = "intel";

  services.xserver.xrandrHeads = [
    "DP1" { output = "DP1"; primary = true; }
    "HDMI2" { monitorConfig = ''Option "Rotate" "left"''; output = "HDMI2"; }
  ];

  system.autoUpgrade = {
    channel = "https://nixos.org/channels/nixos-unstable";
    enable = true;
  };
}
