{ config, lib, pkgs, ... }:

let base = ../.;
in {
  nix = {
    extraOptions = ''
      binary-caches-parallel-connections = 20
      connect-timeout = 10
      experimental-features = nix-command flakes
    '';

    gc = {
      automatic = true;
      dates = "20:00";
      options = "--delete-older-than 14d";
    };

    nrBuildUsers = 4;

    optimise = {
      automatic = true;
      dates = [ "20:30" ];
    };

    package = pkgs.nixVersions.latest;

    settings = {
      allowed-users = [ "nequi" "root" "@wheel" ];
      auto-optimise-store = true;
      cores = 4;
      max-jobs = 4;
      require-sigs = true;
      sandbox = true;
      substituters = [ "https://cache.nixos.org" ];

      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
      ];

      trusted-substituters = [ "http://hydra.nixos.org/" ];
      trusted-users = [ ];
    };
  };

  nixpkgs = {
    system = lib.mkDefault "x86_64-linux";
    config.allowUnfree = true;
  };

  system.stateVersion = "24.11";
}
