{ config, lib, pkgs, ... }:

let secrets = ./ux305c-secrets.nix;
in {
  imports = [
    ./nixos/boot.nix
    ./nixos/common.nix
    ./nixos/docker.nix
    ./nixos/kernel.nix
    ./nixos/networking.nix
    ./nixos/nix.nix
    ./nixos/security.nix
    ./nixos/ssh.nix
    ./nixos/users.nix
    ./nixos/xmonad.nix
    ./nixos/zfs.nix
    ./nixos/zsh.nix
    ./ux305c-hardware.nix
    ./ux305c-secrets.nix
    ./ux305c-wifi.nix
  ];

  environment.systemPackages = with pkgs; [
    firefox-esr
    minecraft

    (import ./nixpkgs/nix-update.nix { pkgs = pkgs; lib = lib; })
  ];

  networking.hostId = "123b567a";
  networking.hostName = "nixus";

  networking.hosts = {
    "127.0.0.1" = [ "${config.networking.hostName}" "localhost" ];
    "10.0.10.10" = [ "serenitybysarah.ca" ];
    "0.0.0.0" = [ "ftp.au.debian.org" ];
  };

  nixpkgs.overlays = [
    (import ./ux305c-overlay.nix {})
  ];

  services.emacs = {
    enable = true;
    package = import ./nixpkgs/emacs.nix { pkgs = pkgs; };
  };

  services.redshift.enable = false;

  services.xserver.videoDriver = "intel";

  system.autoUpgrade.channel = "https://nixos.org/channels/nixos-unstable";

  virtualisation.docker.enable = false;
}
