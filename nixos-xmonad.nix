{ config, lib, pkgs, ... }:
rec {
  environment.systemPackages = with pkgs; [
    haskellPackages.xmobar
  ];

  services.xserver = {
    autorun = true;
    defaultDepth = 24;
    desktopManager.xterm.enable = false;

    displayManager = {
      lightdm = {
        background = "${pkgs.nixos-artwork.wallpapers.simple-blue}/share/artwork/gnome/nix-wallpaper-simple-blue.png";
        enable = true;

        extraSeatDefaults = ''
          greeter-show-manual-login=true
          greeter-hide-users=true
        '';

        greeters.gtk = {
          extraConfig = ''
            default-user-image = ${pkgs.nixos-icons}/share/icons/hicolor/64x64/apps/nix-snowflake.png
            position = 50%,center -300,end
          '';

          theme = {
            name = "Numix";
            package = pkgs.numix-gtk-theme;
          };
        };
      };

      xserverArgs = [ "-logfile" "/var/log/X.log" ];
    };

    enable = true;
    exportConfiguration = true;

    synaptics = {
      enable = true;
      tapButtons = false;
      twoFingerScroll = true;
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = with pkgs.haskellPackages; haskellPackages: [ xmobar ];
    };

    xkbOptions = "ctrl:nocaps";
  };
}
