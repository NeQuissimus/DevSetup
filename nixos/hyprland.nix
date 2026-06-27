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
      withUWSM = true;
      xwayland.enable = true;
    };

    hyprlock.enable = true;
  };

  services = {
    displayManager.sddm = {
      enable = true;

      wayland.enable = true;
    };

    xserver.enable = true;
  };
}
