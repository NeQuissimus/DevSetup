{ config, lib, pkgs, ... }:

let base = ../.;
in {
  nix = {
    allowedUsers = [ "nequi" "root" "@wheel" ];

    autoOptimiseStore = true;
    binaryCaches = [ "https://cache.nixos.org" ];

    binaryCachePublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
    ];

    buildCores = 2;

    extraOptions = ''
      binary-caches-parallel-connections = 20
      connect-timeout = 10
      experimental-features = nix-command flakes
    '';

    gc = {
      automatic = true;
      dates = "20:00";
      options = "--delete-older-than 60";
    };

    maxJobs = 2;

    nixPath = [
      "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
      "nixos-config=${toString base}/ux305c.nix"
    ];

    nrBuildUsers = 2;

    optimise = {
      automatic = true;
      dates = [ "20:30" ];
    };

    package = pkgs.nixUnstable;

    requireSignedBinaryCaches = true;

    trustedBinaryCaches = [ "http://hydra.nixos.org/" ];
    trustedUsers = [ ];

    useSandbox = true;
  };

  nixpkgs = {
    system = "x86_64-linux";
    config.allowUnfree = true;
  };

  system.stateVersion = "20.09";
}
