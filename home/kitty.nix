{ config, pkgs, ... }:
{
    programs.kitty = {
        enable = true;
        enableGitIntegration = true;

        font = {
            size = 12;
            name = builtins.head (config.fonts.fontconfig.defaultFonts.monospace);
        };

        mouseBindings = {
            "shift+left" = "release grabbed,ungrabbed mouse_handle_click link";
        };

        settings = {
            background_blur = 32;
            background_opacity = 0.7;
            confirm_os_window_close = 0;
            enable_audio_bell = false;
            scrollback_lines = 100000;
            shell = "${pkgs.zsh}/bin/zsh -l -c ${pkgs.zellij}/bin/zellij";
            update_check_interval = 0;
        };

        shellIntegration.enableZshIntegration = true;

        themeFile = "Relaxed_Afterglow";
    };
}