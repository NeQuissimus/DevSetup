{ config, lib, pkgs, ... }:

{
  imports = [ ./nixos-common.nix ./nixos-harden.nix ./nixos-xmonad.nix ./ux305c-hardware.nix ./ux305c-wifi.nix ];

  boot.loader.grub = {
    device = "nodev";
    efiSupport = true;
    enable = true;
    version = 2;
  };

  environment.systemPackages = with pkgs; [
    ammonite
    chromium
    keybase-gui
    # kubectl
    # kubernetes-helm
    # minikube
    sbt-extras
  ];

  networking.hostName = "nixus";

  networking.hosts = {
    "127.0.0.1" = ["${config.networking.hostName}" "localhost"];
    "10.0.10.10" = ["serenitybysarah.ca"];
    "0.0.0.0" = ["ftp.au.debian.org"];
  };

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
    kubernetes = (super.kubernetes.override { components = [ "cmd/kubectl" ]; }).overrideAttrs (oldAttrs: {
      version = "1.9.2";
      name = "kubectl-1.9.2";
      src = pkgs.fetchFromGitHub {
        owner = "kubernetes";
        repo = "kubernetes";
        rev = "v1.9.2";
        sha256 = "0yf9k08ngplqb5xwipyfp25nlb19ykh05b7l9qcvybczihdkv6p2";
      };
    });
  };

  programs.ssh.extraConfig = ''
    Host pine
    HostName 10.0.10.10
    User ubuntu
  '';

  services.kbfs = {
    enable = true;
    mountPoint = "/keybase";
    extraFlags = [
      "-label kbfs"
      "-mount-type normal"
    ];
  };

  services.keybase.enable = true;

  services.dnsmasq.servers = [
    "9.9.9.9"
    "10.0.10.6"
  ];

  services.xserver.displayManager.sessionCommands = with pkgs; lib.mkAfter ''
    ${feh}/bin/feh --bg-scale "${nixos-artwork.wallpapers.simple-dark-gray}/share/artwork/gnome/nix-wallpaper-simple-dark-gray.png" &
    ${xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr &
    ${autocutsel}/bin/autocutsel &
    ${autocutsel}/bin/autocutsel -s PRIMARY &
  '';

  services.xserver.resolutions = [
    { x = 1920; y = 1080; }
    { x = 1280; y = 800; }
    { x = 1024; y = 768; }
  ];

  services.xserver.videoDriver = "intel";

  virtualisation.docker.package = pkgs.docker-edge;

  virtualisation.virtualbox.host = {
    enable = false;
  };
}
