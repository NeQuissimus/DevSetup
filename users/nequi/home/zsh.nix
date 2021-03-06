{ config, pkgs, ... }: {
  programs.zsh = {
    enableAutosuggestions = true;
    enableCompletion = true;

    initExtra = ''
      setopt HIST_IGNORE_ALL_DUPS
      setopt HIST_IGNORE_DUPS
      setopt INC_APPEND_HISTORY
      setopt HIST_IGNORE_SPACE
      setopt HIST_SAVE_NO_DUPS
      export CASE_SENSITIVE="false"
      export GPG_TTY="$(tty)"
      export HIST_STAMPS="dd.mm.yyyy"
      export HISTCONTROL="ignoredups"

      bindkey "^[Od" backward-word
      bindkey "^[Oc" forward-word

      function zle-line-init() {
        if (( ''${+terminfo[smkx]} )); then
         echoti smkx
        fi
      }

      function zle-line-finish() {
        if (( ''${+terminfo[rmkx]} )); then
          echoti rmkx
        fi
      }

      unsetopt correct_all

      vterm_printf(){
        if [ -n "$TMUX" ]; then
          # Tell tmux to pass the escape sequences through
          printf "\ePtmux;\e\e]%s\007\e\\" "$1"
        elif [ "''${TERM%%-*}" = "screen" ]; then
          # GNU screen (screen, screen-256color, screen-256color-bce)
          printf "\eP\e]%s\007\e\\" "$1"
        else
          printf "\e]%s\e\\" "$1"
        fi
      }

      if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
        alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
      fi

      function docker_clean() { docker kill $(docker ps -q); docker rm $(docker ps -a -q); }
      function docker_clean_dangling() { docker images -qf dangling=true | xargs -r docker rmi; }
      function docker_clean_images() { docker kill $(docker ps -q); docker rm $(docker ps -a -q); docker rmi -f $(docker images -q); }
      function docker_inspect() { (skopeo inspect docker://"$1" || docker inspect "$1") | jq; }

      # Nix review PRs
      function noxpr() { nix-shell -p nox --run "nox-review pr $1"; }

      # Tools
      function sbt() {
        args="$@"
        if [ -f "${config.home.homeDirectory}/.sbt/repositories" ]; then
          repo="-Dsbt.override.build.repos=true"
        fi
        nix-shell -p openjdk8 -p sbt-extras --command "sbt -J-Xms1G -J-Xmx8G ''${repo} ''${args}";
      }

      function amm() { nix-shell -p ammonite --command "amm"; }

      function gi() { curl -sLw "\n" https://www.toptal.com/developers/gitignore/api/$@ ;}

      [[ -f "${config.home.homeDirectory}/.zshextras" ]] && source "${config.home.homeDirectory}/.zshextras"
    '';

    sessionVariables = {
      _JAVA_AWT_WM_NONREPARENTING = "1";
      BAT_THEME = "Monokai Extended Bright";
      JAVA_HOME = "${pkgs.openjdk8}";
      PATH = "${config.home.homeDirectory}/.local/bin:$PATH";
      TERMINAL = "xterm-256color";
      XZ_DEFAULTS = "-T 0";
      ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE = "fg=239";
    };

    shellAliases = {
      cat = "${pkgs.bat}/bin/bat";
      diff = "diff --color";
      grep = "${pkgs.ripgrep}/bin/rg";
      ls = "${pkgs.exa}/bin/exa";
      nano = "${pkgs.nano}/bin/nano -E -w -c";
      volume = ''
        awk -F"[][]" "/dB/" { print $$2 }" <(${pkgs.alsaUtils}/bin/amixer sget Master)'';
      volume_down = "${pkgs.alsaUtils}/bin/amixer -q sset Master 5%-; volume";
      volume_up = "${pkgs.alsaUtils}/bin/amixer -q sset Master 5%+; volume";
    };

    oh-my-zsh = {
      custom = "${pkgs.spaceship-prompt}/share/zsh";
      enable = true;

      extraConfig = ''
        zstyle :omz:plugins:ssh-agent lifetime 1h

        export DISABLE_UPDATE_PROMPT=true
        export ZSH_AUTOSUGGEST_USE_ASYNC="true"
        export SPACESHIP_CHAR_SYMBOL="λ "
        export SPACESHIP_PROMPT_SEPARATE_LINE=false
        export SPACESHIP_PROMPT_ORDER=(dir git exec_time battery jobs exit_code char)
      '';

      plugins = [ "git" "sudo" ];

      theme = "spaceship";
    };
  };
}
