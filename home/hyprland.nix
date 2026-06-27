{ lib, pkgs, ... }:
let 
  binding_exec = key : command : {
    _args = [
        (lib.generators.mkLuaInline "mod .. \" + ${key}\"")
        (lib.generators.mkLuaInline "hl.dsp.exec_cmd(\"${command}\")")
        { locked = true; }
    ];
  };

  binding_focus = key: direction: {
    _args = [
        (lib.generators.mkLuaInline "mod .. \" + ${key}\"")
        (lib.generators.mkLuaInline "hl.dsp.focus({ direction = \"${direction}\"})")
        { locked = true; }
    ];
  };

  binding_move = key: direction: {
    _args = [
        (lib.generators.mkLuaInline "mod .. \" + SHIFT + ${key}\"")
        (lib.generators.mkLuaInline "hl.dsp.window.move({ direction = \"${direction}\"})")
        { locked = true; }
    ];
  };

  wallpaper = builtins.fetchurl {
    url = "https://live.staticflickr.com/65535/55248904961_bf0319f2ea_5k.jpg";
    sha256 = "0h7jzzc8r61mgxbj7438a5zy8jdghhbnzb25ym5jhhjf076c1i63";
  };
in {
    home.file."hypr/hypridle.conf".text = ''
        general {
            lock_cmd = pidof hyprlock || hyprlock
            before_sleep_cmd = loginctl lock-session    # lock before suspend
            after_sleep_cmd = hyprctl dispatch dpms on
        }

        listener {
            timeout = 600
            on-timeout = loginctl lock-session
        }

        listener {
            timeout = 900
            on-timeout = hyprctl dispatch dpms off
            on-resume = hyprctl dispatch dpms on
        } 
    '';

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
                (binding_exec "BackSpace" "wlogout")
                (binding_exec "L" "hyprlock")
                (binding_exec "D" "hyprlauncher")
                (binding_exec "Return" "kitty")

                (binding_focus "down" "down")
                (binding_focus "left" "left")
                (binding_focus "right" "right")
                (binding_focus "up" "up")

                (binding_move "down" "down")
                (binding_move "left" "left")
                (binding_move "right" "right")
                (binding_move "up" "up")
            ];

            config = {
                debug.disable_logs = true;

                ecosystem.no_update_news = true;

                general = {
                    border_size = 2;

                    col = {
                        active_border = {
                            colors = [
                                "rgba(00ff99ee)"
                            ];
                        };
                    };

                    gaps_in = 1;
                    gaps_out = 10;
                    layout = "master";
                };

                input = {
                    follow_mouse = 1;
                    kb_layout = "us";
                };

                master = {
                    mfact = 0.7;
                    orientation = "left";
                };

                misc = {
                    disable_autoreload = true;
                    disable_hyprland_logo = true;
                    disable_splash_rendering = true;
                };
            };

            exec_cmd = [ 
                "${pkgs.hyprpaper}/bin/hyprpaper"
                "${pkgs.hypridle}/bin/hypridle"
            ];
        };
    };
}
