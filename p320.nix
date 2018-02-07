{ config, lib, pkgs, ... }:

{
  imports = [ ./nixos-common.nix ./nixos-harden.nix ./nixos-xmonad.nix ./p320-hardware.nix ];

  boot.initrd.kernelModules = [
    "tun" # VPN
  ];

  boot.loader.systemd-boot.enable = true;

  boot.kernel.sysctl = {
    "vm.max_map_count" = 262144; # Increase map count for ElasticSearch
  };

  environment.systemPackages = with pkgs; [
    ammonite
    chromium
    hipchat
    nox
    sbt-extras

    ((pkgs.kubernetes.override { components = [ "cmd/kubectl" ]; }).overrideAttrs (oldAttrs: {
      version = "1.9.2";
      name = "kubectl-1.9.2";
      src = fetchFromGitHub {
        owner = "kubernetes";
        repo = "kubernetes";
        rev = "v1.9.2";
        sha256 = "0yf9k08ngplqb5xwipyfp25nlb19ykh05b7l9qcvybczihdkv6p2";
      };
    }))
  ];

  environment.variables.QTWEBENGINE_DISABLE_SANDBOX = "1";

  networking.hostName = "nixus-desktop";

  networking.hosts = {
    "127.0.0.1" = ["${config.networking.hostName}" "localhost"];
    "10.1.108.103"= ["github.internal" "github.esentire.com"];
    "10.1.110.54" = ["artifactory.internal"];
    "10.1.110.113" = ["gems.internal"];
    "10.1.110.130" = ["build-01.internal"];
    "10.1.110.167" = ["jira.esentire.com" "jira.internal"];
    "10.1.110.191" = ["hipchat.esentire.com"];
    "10.1.110.208" = ["jenkins.internal"];
    "10.1.110.243" = ["gerrit.internal"];
    "10.1.110.57" = ["registry.internal" "portus-1.internal"];
    "10.1.110.83" = ["confluence.internal" "confluence.esentire.com"];
    "10.1.114.20" = ["exchange.esentire.com"];
    "10.1.204.234" = ["soc.uat"];
    "10.203.0.31" = ["kubeapi.qa"];
    "10.203.1.101" = ["kubeapi.dev"];
    #"10.203.1.106" = ["kubeapi.dev"];
    #"10.203.1.107" = ["kubeapi.dev"];

    "0.0.0.0" = [ "ftp.au.debian.org" ];
  };

  nixpkgs.config = {
    firefox.enableAdobeFlash = true;
    firefox.enableAdobeFlashDRM = true;
  };

  services.kbfs = {
    enable = true;
    mountPoint = "/keybase";
    extraFlags = [
      "-label kbfs"
      "-mount-type normal"
    ];
  };
  services.keybase.enable = true;

  # List will be flipped
  services.dnsmasq.servers = [
    "64.6.64.6" # Verisign
    "64.6.65.6" # Verisign
    "192.168.114.199" # Internal colo
    "10.1.115.20" # Internal
    "10.1.114.53" # Internal
    "10.3.114.53" # Internal
    "10.3.115.20" # Internal
    "10.1.120.31" # Internal
    "9.9.9.9" # Quad9
  ];

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    permitRootLogin = "no";
  };

  security.pki.certificates = [
    (lib.fileContents ./registry.crt)
  ];

  services.xserver.displayManager.sessionCommands = with pkgs; lib.mkAfter ''
    sleep 3 && ${xlibs.xrandr}/bin/xrandr --output DP1 --crtc 1 --primary --auto --pos 0x0 --output HDMI2 --crtc 2 --rotate left --auto --pos 1920x0
    ${xorg.xsetroot}/bin/xsetroot -solid "#222222" &
    ${xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr &
    ${autocutsel}/bin/autocutsel &
    ${autocutsel}/bin/autocutsel -s PRIMARY &
    ${xlibs.xhost}/bin/xhost + &
  '';

  services.xserver.videoDriver = "intel";

  services.xserver.xrandrHeads = [
    "DP1" { output = "DP1"; primary = true; }
    "HDMI2" { monitorConfig = ''Option "Rotate" "left"''; output = "HDMI2"; }
  ];

  system.autoUpgrade.channel = "https://nixos.org/channels/nixos-unstable";

  users.users.nequi.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDHYnkuOuI4NS9IrEWuq/+QFHLz7JE/ZlvNZT0I2a1wk nequi@nixus"
  ];

  virtualisation.docker.package = pkgs.docker-edge;

  virtualisation.virtualbox.host = {
    enable = false;
  };
}
