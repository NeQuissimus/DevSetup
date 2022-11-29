{ config, pkgs, ... }:
let
  nequi-zsh = pkgs.stdenv.mkDerivation rec {
    pname = "nequi-zsh";
    version = "1.1";

    src = pkgs.fetchFromGitHub {
      owner = "NeQuissimus";
      repo = "nequi-zsh";
      sha256 = "sha256-vlcBsRsMy5GiKO4egpX3vUS8EJTLr2UWLJF1az7rHA8=";
      rev = "v${version}";
    };

    installPhase = ''
      mkdir -p "$out/share/zsh/themes/"
      install -Dm0644 themes/nequissimus.zsh-theme "$out/share/zsh/themes/"
    '';
  };
in
{
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

      # Nix review PRs
      function noxpr() { nix-shell -p nox --run "nox-review pr $1"; }

      function gi() { ${pkgs.curl}/bin/curl -sLw "\n" https://www.toptal.com/developers/gitignore/api/$@ ;}

      [[ -f "${config.home.homeDirectory}/.zshextras" ]] && source "${config.home.homeDirectory}/.zshextras"

      [ -f /opt/dev/dev.sh ] && source /opt/dev/dev.sh
      if [ -e /Users/nequi/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/nequi/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
      [[ -x /usr/local/bin/brew ]] && eval $(/usr/local/bin/brew shellenv)
      [[ -x /opt/homebrew/bin/brew ]] && eval $(/opt/homebrew/bin/brew shellenv)

      [[ -f /opt/dev/sh/chruby/chruby.sh ]] && type chruby >/dev/null 2>&1 || chruby () { source /opt/dev/sh/chruby/chruby.sh; chruby "$@"; }
      export KUBECONFIG=''${KUBECONFIG:+$KUBECONFIG:}/Users/nequi/.kube/config:/Users/nequi/.kube/config.shopify.cloudplatform

      for file in /Users/nequi/src/github.com/Shopify/cloudplatform/workflow-utils/*.bash; do source ''${file}; done
      kubectl-short-aliases

      function podvvm() {
        local line="$(kubectl --context=data-platform-a-us-central1-1 --namespace=pepto-infrastructure-staging-unrestricted get pods -o wide | grep kafka-connect | shuf | head -1)"
        name="$(echo $line | awk '{print $1}')"
        ip="$(echo $line | awk '{print $6}')"

        sudo ifconfig lo0 alias ''${ip}/32
        kubectl port-forward --context=data-platform-a-us-central1-1 --namespace=pepto-infrastructure-staging-unrestricted --address ''${ip} pods/''${name} 9011:9011
        sudo ifconfig lo0 inet ''${ip} delete
      }

      function tryjq() {
        jq -R -r '. as $line | try fromjson catch $line'
      }

      function fixnix() {
        # macOS updates break Nix
        grep -q "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh" /etc/zshrc || echo "if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'; fi" | sudo tee -a /etc/zshrc
        grep -q "trusted-users" /etc/nix/nix.conf || (echo "trusted-users = root $USER" | sudo tee -a /etc/nix/nix.conf && sudo launchctl stop org.nixos.nix-daemon && sudo launchctl start org.nixos.nix-daemon)
      }
    '';

    localVariables = {
      _JAVA_AWT_WM_NONREPARENTING = "1";
      BAT_THEME = "Monokai Extended Bright";
      EDITOR = "nano";
      HOMEBREW_NO_ANALYTICS = "1";
      HOMEBREW_NO_COLOR = "1";
      HOMEBREW_NO_EMOJI = "1";
      JAVA_HOME = "${pkgs.openjdk11}";
      JQ_COLORS = "1;39:0;39:0;39:0;39:0;32:1;39:1;39";
      PATH = "${config.home.homeDirectory}/.local/bin:$PATH";
      TERMINAL = "xterm-256color";
      XZ_DEFAULTS = "-T 0";
      ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE = "fg=3";
    };

    shellAliases = {
      cat = "${pkgs.bat}/bin/bat";
      grep = "${pkgs.ripgrep}/bin/rg";
      ls = "${pkgs.exa}/bin/exa";
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

      plugins = [ "docker" "git" "gpg-agent" "kubectl" "sbt" "scala" "sudo" ];

      theme = "nequissimus";
    };
  };
}
