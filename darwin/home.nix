{ config, pkgs, lib, ... }:

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
in {
  home = {
    packages = with pkgs; [
      curl
      (google-cloud-sdk.withExtraComponents [
        google-cloud-sdk.components.bq
        google-cloud-sdk.components.gke-gcloud-auth-plugin
      ])
      jaq
      kubectl
      nixfmt-classic
    ];

    file = {
      ".nanorc".text = ''
        set linenumbers
        set tabsize 2
        set tabstospaces
        set trimblanks
        set unix

        include ${pkgs.nanorc}/share/*.nanorc
      '';

      ".sbt/1.0/plugins/sbt-updates.sbt".text = ''
        addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.3")
      '';
    };

    stateVersion = "24.05";
  };

  manual = {
    html.enable = false;
    manpages.enable = false;
    json.enable = false;
  };

  news.display = "silent";

  programs = {
    git = {
      aliases = {
        bclean =
          "!(git for-each-ref --format '%(refname:short)' refs/heads | grep -v 'master\\|main' | xargs git branch -D)";
        clear = "clean -dfx";
        fpush = "push --force-with-lease";
        lg =
          "log --all --decorate --color --graph --pretty=format:'%Cred%h%Creset %Cgreen(%cr)%Creset - %s %C(bold blue)<%an>%Creset%C(auto)%d%Creset' --abbrev-commit";
        ll = ''
          log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate --numstat'';
        undo = "reset HEAD~1 --mixed";
      };

      delta = {
        enable = true;

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

      enable = true;

      extraConfig = {
        advice.detachedHead = "false";
        apply.whitespace = "fix";
        branch.autosetuprebase = "always";
        core.commitGraph = "true";
        core.editor = "nano";
        core.whitespace = "fix,-indent-with-non-tab,trailing-space,cr-at-eol";
        diff.algorithm = "patience";
        diff.colorMoved = "default";
        gc.writeCommitGraph = "true";
        help.autocorrect = "5";
        init.defaultBranch = "main";
        merge.renamelimit = "4096";
        pull.rebase = "true";
        push.default = "upstream";
        submodule.recurse = "true";
        tag.sort = "version:refname";
        credential.helper =
          "store --file /opt/dev/var/private/git_credential_store";
        url."https://github.com/Shopify/".insteadOf = [
          "git@github.com:Shopify/"
          "git@github.com:shopify/"
          "ssh://git@github.com/Shopify/"
          "ssh://git@github.com/shopify/"
        ];
      };

      ignores = [
        ".bsp"
        ".metals"
        ".bloop"
        ".scalafix.conf"
        ".scalafmt.conf"
        # https://www.toptal.com/developers/gitignore/api/emacs
        "*~"
        "#*#"
        "/.emacs.desktop"
        "/.emacs.desktop.lock"
        "*.elc"
        "auto-save-list"
        "tramp"
        ".#*"
        ".org-id-locations"
        "*_archive"
        "*_flymake.*"
        "/eshell/history"
        "/eshell/lastdir"
        "/elpa/"
        "*.rel"
        "/auto/"
        ".cask/"
        "dist/"
        "flycheck_*.el"
        "/server/"
        ".projectile"
        ".dir-locals.el"
        "/network-security.data"
      ];

      signing = {
        key = "F09D70D880F01FF54E51477B06A338C29C3CE904";
        gpgPath = "/opt/dev/bin/gpg-auto-pin";
        signByDefault = true;
      };

      userEmail = "tim@nequissimus.com";
      userName = "Tim Steinbach";
    };

    htop = {
      enable = true;
      settings.show_program_path = true;
    };

    home-manager.enable = true;

    jq.enable = true;

    ssh = {
      enable = true;

      extraConfig = ''
        ChallengeResponseAuthentication no
        Ciphers chacha20-poly1305@openssh.com,aes256-gcm@openssh.com,aes128-gcm@openssh.com,aes256-ctr,aes192-ctr,aes128-ctr
        ConnectTimeout 60
        ConnectionAttempts 60
        HostKeyAlgorithms ssh-ed25519-cert-v01@openssh.com,ssh-rsa-cert-v01@openssh.com,ssh-ed25519,ssh-rsa,rsa-sha2-512
        KexAlgorithms curve25519-sha256@libssh.org,diffie-hellman-group-exchange-sha256
        MACs hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com,umac-128-etm@openssh.com,hmac-sha2-512,hmac-sha2-256,umac-128@openssh.com
        PasswordAuthentication no
        PubkeyAuthentication yes
        UseRoaming no
      '';

      hashKnownHosts = true;

      serverAliveInterval = 240;
    };

    vscode = {
      enable = true;
      enableUpdateCheck = false;

      extensions = with pkgs.vscode-extensions; [
        bbenoist.nix
        brettm12345.nixfmt-vscode
        eamodio.gitlens
        github.copilot
        hashicorp.terraform
        ms-python.python
        redhat.java
        scala-lang.scala
        scalameta.metals
        vscjava.vscode-gradle
        vscjava.vscode-java-pack
      ];

      userSettings = {
        "editor.formatOnSave" = true;
        "files.associations" = { "*.ejson" = "json"; };
        "files.watcherExclude" = {
          "**/.bloop" = true;
          "**/.metals" = true;
          "**/.ammonite" = true;
        };
        "github.copilot.editor.enableAutoCompletions" = true;
        "gitlens.launchpad.indicator.enabled" = false;
        "gitlens.statusBar.enabled" = false;
        "gitlens.telemetry.enabled" = false;
        "metals.enableIndentOnPaste" = true;
        "telemetry.telemetryLevel" = "off";
        "workbench.startupEditor" = "none";
      };

      mutableExtensionsDir = false;
    };

    wezterm = {
      enable = true;
      enableZshIntegration = true;

      extraConfig = ''
        local wezterm = require 'wezterm'
        local config = wezterm.config_builder()

        config.color_scheme = 'AfterGlow'
        config.hide_tab_bar_if_only_one_tab = true
        config.window_background_opacity = 0.7
        config.font = wezterm.font 'Fira Code'

        return config
      '';
    };

    zsh = {
      enable = true;

      autosuggestion.enable = true;
      enableCompletion = true;

      initExtra = ''
        export CASE_SENSITIVE="false"
        export GPG_TTY="$(tty)"
        export HIST_STAMPS="dd.mm.yyyy"
        export HISTCONTROL="ignoredups"
        setopt HIST_IGNORE_ALL_DUPS
        setopt HIST_IGNORE_DUPS
        setopt HIST_IGNORE_SPACE
        setopt HIST_SAVE_NO_DUPS
        setopt INC_APPEND_HISTORY

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

        function gi() { ${pkgs.curl}/bin/curl -sLw "\n" https://www.toptal.com/developers/gitignore/api/$@ ;}

        function tryjq() {
          jq -R -r '. as $line | try fromjson catch $line'
        }

        function fixnix() {
          # macOS updates break Nix
          grep -q "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh" /etc/zshrc || echo "if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'; fi" | sudo tee -a /etc/zshrc
          grep -q "trusted-users" /etc/nix/nix.conf || (echo "trusted-users = root $USER" | sudo tee -a /etc/nix/nix.conf && sudo launchctl stop org.nixos.nix-daemon && sudo launchctl start org.nixos.nix-daemon)
        }

        function fixkube() {
          grep -q "nix/profiles/home-manager/home-path/bin/kubectl" /Users/nequi/.kube/config.shopify.cloudplatform || ${pkgs.gnused}/bin/sed -i 's|command: gke-gcloud-auth-plugin|command: /Users/nequi/.local/state/nix/profiles/home-manager/home-path/bin/gke-gcloud-auth-plugin|g' /Users/nequi/.kube/config.shopify.cloudplatform
        }

        function update() {
          nix-channel --update && nix run nix-darwin -- switch --flake /Users/nequi/src/github.com/NeQuissimus/DevSetup/darwin/ --impure && brew update && brew upgrade && (yes | gcloud components update) && exit
        }

        # Load Nix
        #if [ -e /Users/nequi/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/nequi/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

        # Tooling
        [ -f /opt/dev/dev.sh ] && source /opt/dev/dev.sh
        [[ -x /usr/local/bin/brew ]] && eval $(/usr/local/bin/brew shellenv)
        [[ -x /opt/homebrew/bin/brew ]] && eval $(/opt/homebrew/bin/brew shellenv)
        [[ -f /opt/dev/sh/chruby/chruby.sh ]] && { type chruby >/dev/null 2>&1 || chruby () { source /opt/dev/sh/chruby/chruby.sh; chruby "$@"; } }

        # Kubernetes
        export KUBECONFIG=''${KUBECONFIG:+$KUBECONFIG:}/Users/nequi/.kube/config:/Users/nequi/.kube/config.shopify.cloudplatform
        fixkube
        for file in /Users/nequi/src/github.com/Shopify/cloudplatform/workflow-utils/*.bash; do source ''${file}; done
        kubectl-short-aliases
      '';

      localVariables = {
        _JAVA_AWT_WM_NONREPARENTING = "1";
        BAT_THEME = "Monokai Extended Bright";
        EDITOR = "nano";
        HOMEBREW_NO_ANALYTICS = "1";
        HOMEBREW_NO_COLOR = "1";
        HOMEBREW_NO_EMOJI = "1";
        HOMEBREW_NO_ENV_HINTS = "1";
        JQ_COLORS = "1;39:0;39:0;39:0;39:0;32:1;39:1;39";
        PATH =
          "/nix/var/nix/profiles/system/sw/bin:/Users/nequi/.local/state/nix/profiles/home-manager/home-path/bin:/Users/nequi/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/Users/nequi/.local/bin:$PATH";
        TERMINAL = "xterm-256color";
        XZ_DEFAULTS = "-T 0";
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

        plugins =
          [ "docker" "git" "gpg-agent" "kubectl" "mvn" "sbt" "scala" "sudo" ];

        theme = "nequissimus";
      };
    };
  };
}
