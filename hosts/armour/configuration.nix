{
  inputs,
  config,
  pkgs,
  lib,
  ipv4Address,
  ...
}:
let
  bambu = pkgs.bambu-studio.override {
    withNvidiaGLWorkaround = true;
  };
in
{
  imports = [
    ./hardware.nix

    ../../nixos/hyprland.nix
    ../../nixos/logs.nix
    ../../nixos/nix.nix
    ../../nixos/security.nix
    ../../nixos/ssh.nix
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

  environment.systemPackages =
    (with pkgs; [
      git
      godot
      kdePackages.dolphin
      kdePackages.qtsvg
      orca-slicer
      prismlauncher
      tor-browser
      vlc
    ])
    ++ [
      bambu
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

    firewall.allowedTCPPorts = [
      9002
      12345
    ];

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

  services = {
    prometheus.exporters = {
      node = {
        enable = true;
        enabledCollectors = [ "systemd" ];
        port = 9002;
      };
    };

    xserver.videoDrivers = [ "nvidia" ];
  };

  system = {
    autoUpgrade.allowReboot = lib.mkForce false;
    stateVersion = lib.mkForce "26.05";
  };
}
