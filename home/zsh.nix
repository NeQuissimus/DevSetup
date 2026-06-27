{ config, pkgs, ... }:
let
  nequi-zsh = pkgs.stdenvNoCC.mkDerivation rec {
    pname = "nequi-zsh";
    version = "1.2";

    src = pkgs.fetchFromGitHub {
      owner = "NeQuissimus";
      repo = "nequi-zsh";
      sha256 = "sha256-dP1lPI+1y9mJ7JWY3NEfvkHVoYjVeHzJAI3Mzw8h3RE=";
      rev = "v${version}";
    };

    installPhase = ''
      mkdir -p "$out/share/zsh/themes/"
      install -Dm0644 themes/nequissimus.zsh-theme "$out/share/zsh/themes/"
    '';
  };
in
{
  programs = {
    delta = {
      enable = true;
      enableGitIntegration = true;

      options = {
        decorations = {
          commit-decoration-style = "bold yellow box ul";
          file-style = "bold yellow ul";
          file-decoration-style = "none";
        };

        features = "line-numbers";
        inspect-raw-lines = "false";
        whitespace-error-style = "22 reverse";
      };
    };

    zsh = {
      enable = true;

      autosuggestion.enable = true;
      enableCompletion = true;

      initContent = ''
        export CASE_SENSITIVE="false"
        export GPG_TTY="$(tty)"
        export HIST_STAMPS="dd.mm.yyyy"
        export HISTCONTROL="ignoredups"
        export HISTSIZE=25000
        export SAVEHIST=25000
        export HISTDUP="erase"

        setopt correct
        setopt numericglobsort

        setopt appendhistory
        setopt sharehistory
        setopt hist_ignore_space
        setopt hist_ignore_all_dups
        setopt hist_save_no_dups
        setopt hist_ignore_dups
        setopt hist_find_no_dups

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

        ${pkgs.fastfetch}/bin/fastfetch
      '';

      sessionVariables = {
        BAT_THEME = "Monokai Extended Bright";
        EDITOR = "codium";
        HOMEBREW_NO_ANALYTICS = "1";
        HOMEBREW_NO_COLOR = "1";
        HOMEBREW_NO_EMOJI = "1";
        HOMEBREW_NO_ENV_HINTS = "1";
        JQ_COLORS = "1;39:0;39:0;39:0;39:0;32:1;39:1;39";
        #PATH = "/nix/var/nix/profiles/system/sw/bin:${config.home.homeDirectory}/.local/state/nix/profiles/home-manager/home-path/bin:${config.home.homeDirectory}/.nix-profile/bin:/nix/var/nix/profiles/default/bin:${config.home.homeDirectory}/.local/bin:$PATH";
        TERM = "xterm";
        TERMINAL = "xterm-256color";
        XZ_DEFAULTS = "-T 0";
      };

      localVariables = {
        ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE = "fg=3";
      };

      shellAliases = {
        cat = "${pkgs.bat}/bin/bat";
        grep = "${pkgs.ripgrep}/bin/rg";
        ls = "${pkgs.eza}/bin/eza";
        nano = "${pkgs.nano}/bin/nano -E -w -c";
      };

      oh-my-zsh = {
        custom = "${nequi-zsh}/share/zsh";
        enable = true;

        extraConfig = ''
          zstyle :omz:plugins:ssh-agent lifetime 1h

          export DISABLE_UPDATE_PROMPT=true
          export ZSH_AUTOSUGGEST_USE_ASYNC="true"
          export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE="300"
          export ZSH_AUTOSUGGEST_MANUAL_REBIND=""
          export ZSH_AUTOSUGGEST_STRATEGY=("history" "completion")
        '';

        plugins = [
          "docker"
          "git"
          "gpg-agent"
          "gradle"
          "kubectl"
          "mvn"
          "sbt"
          "scala"
          "sudo"
        ];

        theme = "nequissimus";
      };
    };
  };
}
