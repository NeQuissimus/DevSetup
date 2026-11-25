{ config, pkgs, lib, ... }:
let
  waybar_css = pkgs.fetchurl {
    url = "https://gist.githubusercontent.com/nalakawula/c888bf832277d008cee11cacbe92d0db/raw/76ea3704d9e450d0fc8ba35d2a1449a5458b5c5a/style.css";
    sha256 = "sha256-aDcs460yXHIgRuFfrrLcgLKKPoROwQmvbP9lbMKxinU=";
  };

  waybar_config = pkgs.fetchurl {
   url = "https://gist.githubusercontent.com/nalakawula/c888bf832277d008cee11cacbe92d0db/raw/76ea3704d9e450d0fc8ba35d2a1449a5458b5c5a/config.jsonc";
   sha256 = "sha256-M3K7up9z/ITqeXl+iO6sSVaLljoIKI3fLecRRSec+O4=";
  };

  waybar_config_patched = pkgs.runCommand "waybar-config-patched.jsonc" {
    nativeBuildInputs = [ pkgs.perl ];
    src = waybar_config;
  } ''
    cp $src config.jsonc
    chmod +w config.jsonc   # store paths are read-only, so work on a copy
    perl -CSD -i -pe 's/\x{E732}/\x{F313}/g' config.jsonc

    # Verification: fail the build unless patched properly.
    # Reads all input, exits nonzero if no snowflake found.
    perl -CSD -0777 -ne 'exit((/\x{F313}/g) ? 0 : 1)' config.jsonc
    # And the inverse: must NOT contain the Arch glyph anymore.
    perl -CSD -0777 -ne 'exit((/\x{E732}/g) ? 1 : 0)' config.jsonc    
   
    cp config.jsonc $out
  '';
in {
  home = {
    file.".config/waybar/config".source = waybar_config_patched;

    packages = with pkgs; [
      hyprlauncher
      kitty
    ];
  };

  programs.swaylock.enable = true;
  programs.waybar = {
    enable = true;
    style = waybar_css;
  };

  wayland.windowManager.niri = {
    enable = true;
    enableDefaultConfig = false;

    settings = {
        binds = {
            "Mod+Return" = {
              _props.repeat = false;
              spawn = ["kitty"];
            };
            "Mod+D" = { spawn = ["hyprlauncher"]; };
            "Mod+Shift+L" = { spawn = ["swaylock"]; };
            "Mod+Left" = { focus-column-left = {}; };
            "Mod+Right" = { focus-column-right = {}; };
            "Mod+Up" = { set-column-width = "+10%"; };
            "Mod+Down" = { set-column-width = "-10%"; };
            "Mod+Shift+Up" = { maximize-window-to-edges = {}; };
            "Mod+Shift+Left" = { move-column-left = {}; };
            "Mod+Shift+Right" = { move-column-right = {}; };
        };

        _children = [
          { window-rule._children = [
            { draw-border-with-background = false; }
          ]; }
        ];
    };

    systemd.enable = true;
  };
}
