{ config, lib, pkgs, ... }:

{
  boot = {
    enableContainers = false;

    cleanTmpDir = true;
  };

  console.keyMap = "us";

  documentation.nixos.enable = false;

  environment = {
    sessionVariables = {
      TERMINFO_DIRS = "/run/current-system/sw/share/terminfo";
    };

    systemPackages = with pkgs; [
      # Basics
      alacritty
      alsaUtils
      autocutsel
      bat
      binutils
      conky
      dnsutils
      exa
      feh
      firefox-esr
      git
      gitAndTools.hub
      gnupg1compat
      htop
      jq
      oh-my-zsh
      ripgrep
      rofi
      skopeo

      ((pkgs.callPackage ../nixpkgs/nix-home.nix) { })
    ];
  };

  fonts = {
    enableFontDir = true;

    fonts = with pkgs; [
      dejavu_fonts
      emacs-all-the-icons-fonts
      font-awesome
      hasklig
      powerline-fonts
    ];

    fontconfig.defaultFonts.monospace = [ "DejaVu Sans Mono for Powerline" ];
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    pulseaudio.enable = true;
  };

  i18n = { defaultLocale = "en_CA.UTF-8"; };

  location = {
    latitude = 43.18;
    longitude = -80.38;
  };

  powerManagement = {
    enable = true;
    powerUpCommands = ''
      ${pkgs.hdparm}/sbin/hdparm -B 255 /dev/sda
    '';
  };

  programs = {
    gnupg.agent.enable = true;

    slock.enable = true;
  };

  services = {
    fwupd.enable = true;

    logind.extraConfig = ''
      RuntimeDirectorySize=20%
    '';

    redshift = {
      enable = lib.mkDefault true;
      temperature.night = 1900;
    };

    tlp = {
      enable = true;
      settings = { DISK_APM_LEVEL_ON_BAT = "254 254"; };
    };

    upower.enable = true;

    xserver.displayManager.sessionCommands = with pkgs;
      lib.mkAfter ''
        ${feh}/bin/feh --bg-scale "${nixos-artwork.wallpapers.simple-dark-gray}/share/artwork/gnome/nix-wallpaper-simple-dark-gray.png" &
        ${conky}/bin/conky -c ~/.conky &
        ${xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr &
        ${autocutsel}/bin/autocutsel &
        ${autocutsel}/bin/autocutsel -s PRIMARY &
      '';
  };

  system.autoUpgrade = {
    channel = lib.mkDefault "https://nixos.org/channels/nixos-20.09";
    dates = "9:00";
    enable = lib.mkDefault false;
  };

  time = { timeZone = "America/Toronto"; };
}
