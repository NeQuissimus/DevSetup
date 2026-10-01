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

  awakenedpoetrade = pkgs.awakened-poe-trade.overrideAttrs (oldAttrs: {
    postFixup = ''
      makeWrapper ${lib.getExe pkgs.electron} $out/bin/awakened-poe-trade \
        --add-flags "$out/share/awakened-poe-trade/resources/app.asar --no-sandbox --disable-gpu --force-device-scale-factor=1 --ozone-platform=x11 --no-overlay" \
        --prefix LD_LIBRARY_PATH : "${
          lib.makeLibraryPath [
            pkgs.libxtst
            pkgs.libxt
          ]
        }" \
    '';
  });
in
{
  imports = [
    ./hardware.nix

    ../../nixos/brave.nix
    ../../nixos/helium.nix
    ../../nixos/hyprland.nix
    ../../nixos/logs.nix
    ../../nixos/nix.nix
#    ../../nixos/ollama.nix
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

    kernelParams = [
      "loglevel=3"
      "quiet"
      "splash"
    ];

    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
    };
  };

  environment.systemPackages =
    (with pkgs; [
      brave-origin
      git
      godot
      kdePackages.dolphin
      kdePackages.qtsvg
      prismlauncher
      tor-browser
      vlc
    ])
    ++ (with inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}; [
      omp
    ])
    ++ [
      awakenedpoetrade
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
      package = config.boot.kernelPackages.nvidiaPackages.latest;
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

  nixpkgs.overlays = [
    inputs.nix-cachyos-kernel.overlays.default
  ];

  programs.thunderbird.enable = true;

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
