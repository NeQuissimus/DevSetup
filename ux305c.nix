{ config, lib, pkgs, ... }:

{
  imports = [ ./nixos-common.nix ./nixos-common-1803.nix ./nixos-harden.nix ./nixos-xmonad.nix ./nixos-zfs.nix ./ux305c-hardware.nix ./ux305c-wifi.nix ];

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

  services.kbfs.enable = false;
  services.keybase.enable = false;

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

#  services.xserver.resolutions = [
#    { x = 1920; y = 1080; }
#    { x = 1280; y = 800; }
#    { x = 1024; y = 768; }
#  ];

  services.xserver.videoDriver = "intel";

  virtualisation.docker.package = pkgs.docker-edge;
}
