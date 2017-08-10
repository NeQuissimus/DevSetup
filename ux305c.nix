{ config, lib, pkgs, ... }:

{
  imports = [ ./nixos-common.nix ./ux305c-hardware.nix ./xmonad-config.nix ./ux305c-wifi.nix ];

  boot.initrd.luks.devices = [{
    allowDiscards = true;
    device = "/dev/disk/by-uuid/44dd41f0-7642-4939-9118-0caec4498739";
    name = "root";
    preLVM = true;
  }];

  boot.loader.grub = {
    device = "nodev";
    efiSupport = true;
    enable = true;
    version = 2;
  };

  networking.hostName = "nixus";

  networking.hosts = {
    "127.0.0.1" = ["${config.networking.hostName}" "localhost"];
    "0.0.0.0" = ["ftp.au.debian.org"];
  };

  services.cron = {
    enable = true;

    systemCronJobs = [
      "15 18 * * 6 root fstrim -v -a"
    ];
  };

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
}
