{ config, pkgs, lib, ... }:
{
  imports = [
    ../../nixos/kernel.nix
    ../../nixos/nix.nix
    ../../nixos/security.nix
    ../../nixos/users.nix
    ../../nixos/zfs.nix
    ../../nixos/zsh.nix
  ];

  boot = {
    kernelPackages = pkgs.cachyosKernels.linuxPackages-cachyos-lts;
    supportedFilesystems.zfs = true;
    zfs.package = config.boot.kernelPackages.zfs_cachyos;
  };

  hardware = {
    bluetooth.enable = true;

    graphics = {
      enable = true;
      enable32Bit = true;
    };
  };

  nix.settings = {
    substituters = [ "https://attic.xuyh0120.win/lantian" ];
    trusted-public-keys = [ "lantian:EeAUQ+W+6r7EtwnmYjeVwx5kOGEBpjlBfPlzGlTNvHc=" ];
  };

  nixpkgs = {
    config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
      "steam"
      "steam-unwrapped"
    ];

    overlays = [
      nix-cachyos-kernel.overlays.default
    ];
  }

  programs = {
    firefox = {
      enable = true;
      languagePacks = [ "en-CA" "en-US" ];
      package = pkgs.librewolf;

      policies = {
        DisableFirefoxStudies = true;

        ExtensionSettings = builtins.listToAttrs (builtins.map (id: {
          name = id;
          value = {
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/${id}/latest.xpi";
            installation_mode = "force_installed";
          };
        }) [ "7tv-extension" "twitch-auto-points" "ublock-origin" "vertical-twitch" ]);

        SearchEngines = {
          Default = "Ecosia";

          Remove = [
              "eBay"
              "Google"
              "Bing"
              "Wikipedia"
              "Perplexity"
          ];
        };


      };

      preferencesStatus = "locked";
    };

    gamemode.enable = true;

    steam = {
      enable = true;

      extraCompatPackages = with pkgs; [
        proton-ge-bin
      ];
    };
  };

  system.stateVersion = "26.05";
}