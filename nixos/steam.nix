{ lib, pkgs, ... }:
{
  boot.kernel.sysctl = {
    "kernel.unprivileged_userns_clone" = lib.mkForce 1;
  };

  nixpkgs = {
    config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
      "steam"
      "steam-unwrapped"
    ];
  };

  programs = {
    gamemode.enable = true;

    steam = {
      enable = true;

      extraCompatPackages = with pkgs; [
        proton-ge-bin
      ];
    };
  };
}