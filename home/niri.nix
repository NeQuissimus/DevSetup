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

  colours = {
    dark = "#404041";
    accent = "#e6c17a";
    light = "#f6ede3";
  };

  wallpaper = builtins.fetchurl {
    url = "https://live.staticflickr.com/65535/55248904961_bf0319f2ea_5k.jpg";
    sha256 = "0h7jzzc8r61mgxbj7438a5zy8jdghhbnzb25ym5jhhjf076c1i63";
  };
in {
  home = {
    file.".config/waybar/config".source = waybar_config_patched;

    packages = with pkgs; [
      oreo-cursors-plus
    ];

    sessionVariables.XCURSOR_PATH = "/etc/profiles/per-user/${config.home.username}/share/icons";
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
              spawn = ["${pkgs.kitty}/bin/kitty"];
            };
            "Mod+D" = { spawn = ["${pkgs.hyprlauncher}/bin/hyprlauncher"]; };
            "Mod+Shift+L" = { spawn = ["${pkgs.swaylock}/bin/swaylock"]; };
            "Mod+Left" = { focus-column-left = {}; };
            "Mod+Right" = { focus-column-right = {}; };
            "Mod+Up" = { set-column-width = "+10%"; };
            "Mod+Down" = { set-column-width = "-10%"; };
            "Mod+Shift+Up" = { maximize-window-to-edges = {}; };
            "Mod+Shift+Left" = { move-column-left = {}; };
            "Mod+Shift+Right" = { move-column-right = {}; };
        };

        cursor = {
          hide-after-inactive-ms = 5000;
          xcursor-theme = "oreo_spark_orange_cursors";
          xcursor-size = 48;
        };

        hotkey-overlay = { "skip-at-startup" = {}; };

        layout = {
          background-color = colours.light;

          focus-ring = {
            active-color = colours.light;
            inactive-color = colours.dark;
            off = {};
            urgent-color = colours.accent;
            width = 5;
          };

          gaps = 5;
        };


        _children = [
          { spawn-at-startup = "${pkgs.awww}/bin/awww-daemon"; }
          { spawn-at-startup = [ (lib.getExe pkgs.awww) "img" wallpaper ]; }
          { spawn-at-startup = "${lib.getExe pkgs.waybar}"; }

          { prefer-no-csd = {}; }

          { window-rule._children = [
            { match._props = { app-id = "kitty"; }; }
            { background-effect = { xray = true; blur = true; }; }
          ]; }
          { window-rule._children = [
            { background-effect = { blur = true; }; }
            { draw-border-with-background = false; }
          ]; }
        ];
    };

    systemd.enable = true;
  };
}
