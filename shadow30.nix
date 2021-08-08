{ config, lib, pkgs, ... }:

{
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
    ./shadow30-hardware.nix
    ./extras/shadow30-secrets.nix
  ];

  environment.memoryAllocator.provider = "libc";

  environment.systemPackages = with pkgs; [
    (import ./nixpkgs/kconfig-hardened-check.nix { inherit pkgs; })
    (import ./nixpkgs/nix-update.nix { inherit pkgs; })
    (import ./nixpkgs/mdloader.nix { inherit pkgs; })
  ];

  networking.hostId = "123abcde";
  networking.hostName = "nixus";

  networking.hosts = {
    "127.0.0.1" = [ "${config.networking.hostName}" "localhost" ];
    "10.0.10.10" = [ "serenitybysarah.ca" ];
    "0.0.0.0" = [ "ftp.au.debian.org" ];
  };

  services.redshift.enable = false;

  system.autoUpgrade = {
    channel = "https://nixos.org/channels/nixos-unstable";
    enable = true;
  };
}
