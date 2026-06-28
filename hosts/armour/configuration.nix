{
  inputs,
  config,
  pkgs,
  lib,
  ipv4Address,
  ...
}:
{
  imports = [
    ./hardware.nix

    ../../nixos/hyprland.nix
    ../../nixos/nix.nix
    ../../nixos/security.nix
    ../../nixos/steam.nix
    ../../nixos/time.nix
    ../../nixos/users.nix
    ../../nixos/zsh.nix
  ];

  boot = {
    kernelModules = [
      "nvidia"
      "nvidia_modeset"
      "nvidia_drm"
      "nvidia_uvm"
    ];
    kernelPackages = pkgs.cachyosKernels.linuxPackages-cachyos-latest;

    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    bambu-studio
    git
    kdePackages.dolphin
    kdePackages.qtsvg
    prismlauncher
    tor-browser
    vlc
  ];

  hardware = {
    bluetooth.enable = true;

    graphics = {
      enable = true;
      enable32Bit = true;
      extraPackages = with pkgs; [
        mesa
      ];
    };

    nvidia = {
      modesetting.enable = true;
      open = false;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
    };
  };

  networking = {
    defaultGateway = "10.0.0.2";

    firewall.allowedTCPPorts = [ 12345 ];

    hostName = "armour";
    useDHCP = true;
    usePredictableInterfaceNames = true;
  };

  nix.settings = {
    extra-substituters = [ "https://attic.xuyh0120.win/lantian" ];
    extra-trusted-public-keys = [ "lantian:EeAUQ+W+6r7EtwnmYjeVwx5kOGEBpjlBfPlzGlTNvHc=" ];
  };

  nixpkgs.overlays = [
    inputs.nix-cachyos-kernel.overlays.default
  ];

  services.xserver.videoDrivers = [ "nvidia" ];

  system.stateVersion = lib.mkForce "26.05";
}
