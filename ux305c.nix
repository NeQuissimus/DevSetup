{ config, lib, pkgs, ... }:

let
  esentire-dns = import ./esentire-dns.nix;
in {
  imports = [ ./nixos-common.nix ./nixos-harden.nix ./nixos-xmonad.nix ./nixos-zfs.nix ./ux305c-hardware.nix ./ux305c-wifi.nix ];

  boot = {
    loader = {
      systemd-boot.enable = true;
    };

    plymouth.enable = false;

    supportedFilesystems = [ "exfat" "zfs" ];
  };

  environment.systemPackages = with pkgs; [
    slack-dark
  ];

  networking.hostId = "123b567a";
  networking.hostName = "nixus";

  networking.hosts = {
    "127.0.0.1" = ["${config.networking.hostName}" "localhost"];
    "10.0.10.10" = ["serenitybysarah.ca"];
    "172.16.0.254" = ["wifi.esentire.com"];
    "0.0.0.0" = ["ftp.au.debian.org"];
  } // esentire-dns.hosts;

  programs.ssh.extraConfig = ''
    Host pine
    HostName 10.0.10.10
    User ubuntu

    Host router
    HostName 10.0.0.2
    User nequi

    Host pi
    HostName 10.0.10.6
    User pi

    Host cyclops
    HostName 10.0.0.52
    User nequi
  '';

  services.emacs = {
    enable = true;
    package = import ./nixpkgs/emacs.nix { pkgs = pkgs; };
  };

  services.dnsmasq.servers = [
    "10.0.0.2" # Router
    "10.1.115.20" # eSentire
  ];

  services.xserver.videoDriver = "intel";

  services.zfs.autoScrub.enable = true;
}
