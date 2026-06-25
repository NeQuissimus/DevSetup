{ lib, pkgs, ... }:
let 
  binding_exec = key : command : {
    _args = [
        (lib.generators.mkLuaInline "mod .. \" + ${key}\"")
        (lib.generators.mkLuaInline "hl.dsp.exec_cmd(\"${command}\")")
        { locked = true; }
    ];
  };

  wallpaper = builtins.fetchurl {
    url = "https://live.staticflickr.com/65535/55248904961_bf0319f2ea_5k.jpg";
    sha256 = "0h7jzzc8r61mgxbj7438a5zy8jdghhbnzb25ym5jhhjf076c1i63";
  };
in {
    programs.wlogout = {
        enable = true;

        layout = [
            {
                label = "reboot";
                action = "systemctl reboot";
                text = "Reboot";
                keybind = "r";
            }
            {
                label = "shutdown";
                action = "systemctl poweroff";
                text = "Shutdown";
                keybind = "p";
            }
        ];
    };

    services = {
        hyprpaper = {
            enable = true;
            settings = {
                splash = false;
                wallpaper = [{
                    monitor = "";
                    path = "${wallpaper}";
                }];
            };
        };

        hyprsunset = {
            enable = true;
            settings = {
                "07:30" = {
                    temperature = 6500;
                    gamma = 1.0;
                };
                "18:30" = {
                    temperature = 4500;
                    gamma = 0.9;
                };
            };
        };
    };

    wayland.windowManager.hyprland = {
        enable = true;
        package = null; # Use NixOS module
        portalPackage = null;

        settings = {
            mod._var = "SUPER";

            bind = [
                (binding_exec "R" "hyprlauncher")
                (binding_exec "L" "hyprlock")
                (binding_exec "Return" "kitty")
                (binding_exec "BackSpace" "wlogout")
            ];

            exec_cmd = [ "${pkgs.hyprpaper}/bin/hyprpaper" ];
        };
    };
}
