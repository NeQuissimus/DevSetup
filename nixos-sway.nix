{ config, lib, pkgs, ... }: rec {
  config.programs.sway = {
    enable = true;
  };

  config.hardware.opengl = {
    enable = true;
    extraPackages = [ pkgs.vaapiIntel ];
  };
}
