{
  config,
  lib,
  pkgs,
  ...
}:

{
  environment.systemPackages = with pkgs; [
    hyprlauncher
  ];

  programs = {
    hyprland = {
      enable = true;
      withUWSM = false;
      xwayland.enable = true;
    };
  };

  services = {
    displayManager.sddm.enable = false;

    greetd = {
      enable = true;
      settings = {
        default_session = {
          command = "${pkgs.tuigreet}/bin/tuigreet --time --cmd start-hyprland --user-menu";
          user = "nequi";
        };
      };
    };

    xserver = {
      enable = true;
    };
  };
}
