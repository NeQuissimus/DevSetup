{ config, lib, pkgs, ... }:

let secrets = ./extras/ux305c-secrets.nix;
in {
  imports = [
    <nixpkgs/nixos/modules/profiles/hardened.nix>

    ./nixos/boot.nix
    ./nixos/common.nix
    ./nixos/firejail.nix
    ./nixos/kernel.nix
    ./nixos/networking.nix
    ./nixos/nix.nix
    ./nixos/security.nix
    ./nixos/ssh.nix
    ./nixos/users.nix
    ./nixos/xmonad.nix
    ./nixos/zfs.nix
    ./ux305c-hardware.nix
    ./extras/ux305c-secrets.nix
    ./extras/ux305c-wifi.nix
  ];

  environment.memoryAllocator.provider = "jemalloc";

  environment.systemPackages = with pkgs; [
    minecraft

    (import ./nixpkgs/nix-update.nix { inherit pkgs; })
    (import ./nixpkgs/kconfig-hardened-check.nix { inherit pkgs; })
  ];

  networking.hostId = "123b567a";
  networking.hostName = "nixus";

  networking.hosts = {
    "127.0.0.1" = [ "${config.networking.hostName}" "localhost" ];
    "10.0.10.10" = [ "serenitybysarah.ca" ];
    "0.0.0.0" = [ "ftp.au.debian.org" ];
  };

  nixpkgs.overlays = [ (import ./ux305c-overlay.nix { }) ];

  services.journalbeat = {
    enable = true;
    extraConfig = ''
      logging.metrics.enabled: false

      journalbeat.inputs:
        - paths: []

      output.logstash:
        hosts: ["10.0.10.26:9515"]
    '';
    package = pkgs.journalbeat7;
  };

  services.redshift.enable = false;

  services.xserver.videoDriver = "intel";

  system.autoUpgrade = {
    channel = "https://nixos.org/channels/nixos-unstable";
    enable = true;
  };
}
