{ pkgs, config, ... }:
let
  zellij-zjstatus = import ./zellij/status.nix { inherit pkgs; };
in {
  home = {
    file = {
      ".config/zellij/config.kdl".text =
        builtins.replaceStrings [ "@HOME@" ] [ config.home.homeDirectory ]
          (builtins.readFile ./zellij/config.kdl);
      ".config/zellij/layouts/default.kdl".text = ''
          layout {
            default_tab_template {
                children
                pane size=1 borderless=true {
                    plugin location="file:${
                      builtins.unsafeDiscardStringContext zellij-zjstatus
                    }/share/zellij/zjstatus.wasm" {
                    // KEEP "false". Setting "true" makes zjstatus poll
                    // get_session_list, which on zellij >=0.44.1 creates a
                    // render feedback loop -> flicker on new sessions
                    // (dj95/zjstatus#174, zellij-org/zellij#5063).
                    hide_frame_for_single_pane "false"
                    show_startup_tips "false"

                    format_left  "{mode}#[fg=#89B4FA,bg=#181825,bold] {session}#[bg=#181825] {tabs}"
                    format_right "{datetime}"
                    format_space "#[bg=#181825]"

                    mode_normal          "#[bg=#89B4FA] "
                    mode_tmux            "#[bg=#ffc387] "
                    mode_default_to_mode "tmux"

                    tab_normal               "#[fg=#6C7086,bg=#181825] {name} {fullscreen_indicator}{sync_indicator}{floating_indicator}"
                    tab_active               "#[fg=#9399B2,bg=#181825,bold,italic] {index} {name} {fullscreen_indicator}{sync_indicator}{floating_indicator}"
                    tab_fullscreen_indicator "□ "
                    tab_sync_indicator       "  "
                    tab_floating_indicator   "󰉈 "

                    datetime          "#[fg=#9399B2,bg=#181825] {format} "
                    datetime_format   "%A, %d %b %Y %H:%M"
                    datetime_timezone "America/Toronto"
                    }
                }
            }
        }
      '';
    };

    packages = with pkgs; [ zellij-zjstatus ];
  };

  programs.zellij = {
    # https://github.com/nix-community/home-manager/issues/5017
    attachExistingSession = false;
    enable = true;
    enableZshIntegration = false;
    exitShellOnExit = false;
  };
}