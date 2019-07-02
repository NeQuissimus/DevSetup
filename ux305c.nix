{ config, lib, pkgs, ... }:

{
  imports = [ ./nixos-common.nix ./nixos-harden.nix ./nixos-xmonad.nix ./ux305c-hardware.nix ./ux305c-wifi.nix ];

  boot.loader.grub = {
    device = "nodev";
    efiSupport = true;
    enable = true;
    version = 2;
  };

  networking.hostName = "nixus";

  networking.hosts = {
    "127.0.0.1" = ["${config.networking.hostName}" "localhost"];
    "10.0.10.10" = ["serenitybysarah.ca"];
    "172.16.0.254" = ["wifi.esentire.com"];
    "0.0.0.0" = ["ftp.au.debian.org"];
  };

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
    "9.9.9.9"
    "10.0.0.2"
#    "10.0.10.6"
  ];

  services.xserver.videoDriver = "intel";

  system.autoUpgrade = {
    channel = "https://nixos.org/channels/nixos-unstable";
    enable = true;
  };
}
