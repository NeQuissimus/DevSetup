{ config, pkgs, ... }: {
  programs.fish = {
    functions = {
      fish_greeting = "";

      gi = ''
        ${pkgs.curl}/bin/curl -sLw "\n" https://www.toptal.com/developers/gitignore/api/$@'';

      sbt = ''
        set args "$@"
        set repo ""

        if test -f "${config.home.homeDirectory}/.sbt/repositories"
          set repo "-Dsbt.override.build.repos=true"
        end

        ${pkgs.nix}/bin/nix-shell -p openjdk8 -p sbt-extras --command "sbt -J-Xms1G -J-Xmx8G ''${repo} ''${args}";
      '';

      vterm_printf = ''printf "\e]%s\e\\" "$1"'';
    };

    interactiveShellInit = ''
      if test "$INSIDE_EMACS" = 'vterm'
        alias clear 'vterm_printf "51;Evterm-clear-scrollback";tput clear'
      end
    '';

    plugins = [{
      name = "pure";
      src = pkgs.fetchFromGitHub {
        owner = "pure-fish";
        repo = "pure";
        rev = "69e9a074125ad853aae244ce2aabc33811b99970";
        sha256 = "sha256-vVOL/gMJ1BlXQyZqeBRC6zS3E9MGpcIPPFeggmgxMPQ=";
      };
    }];

    promptInit = ''
      set -gx _JAVA_AWT_WM_NONREPARENTING "1"
      set -gx JAVA_HOME "${pkgs.openjdk8}"
      set -gx PATH "${config.home.homeDirectory}/.local/bin:$PATH"
      set -gx TERMINAL "xterm-256color"
      set -gx XZ_DEFAULTS "-T 0"

      set -gx pure_symbol_prompt "λ"
    '';

    shellAliases = {
      cat = "${pkgs.bat}/bin/bat";
      diff = "${pkgs.diffutils}/bin/diff --color";
      grep = "${pkgs.ripgrep}/bin/rg";
      ls = "${pkgs.exa}/bin/exa";
      nano = "${pkgs.nano}/bin/nano -E -w -c";
      volume_down = "${pkgs.alsaUtils}/bin/amixer -q sset Master 5%-; volume";
      volume_up = "${pkgs.alsaUtils}/bin/amixer -q sset Master 5%+; volume";
    };
  };
}
