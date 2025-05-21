{ config, pkgs, lib, ... }:

let
  email = "tim.steinbach@shopify.com";
  gpgKey = "F09D70D880F01FF54E51477B06A338C29C3CE904";
  name = "Tim Steinbach";
  username = "nequi";

  mkAgent = { name }: {
    enable = true;

    config = {
      ProgramArguments =
        [ "/usr/bin/open" "~/Applications/Home Manager Trampolines/$name.app" ];

      KeepAlive = {
        Crashed = true;
        SuccessfulExit = true;
      };

      RunAtLoad = true;
    };
  };

  alfred = (import ./apps/alfred.nix {
    fetchurl = pkgs.fetchurl;
    stdenvNoCC = pkgs.stdenvNoCC;
    undmg = pkgs.undmg;
    inherit lib;
  });

  nequi-zsh = pkgs.stdenv.mkDerivation rec {
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

  vscode_nord = {
    name = "nord-visual-studio-code";
    publisher = "arcticicestudio";
    version = "0.19.0";
    sha256 = "sha256-awbqFv6YuYI0tzM/QbHRTUl4B2vNUdy52F4nPmv+dRU=";
  };

  zellij-monocle = pkgs.stdenv.mkDerivation rec {
    pname = "zellij-monocle";
    version = "0.100.2";

    src = pkgs.fetchurl {
      url =
        "https://github.com/imsnif/monocle/releases/download/v0.100.2/monocle.wasm";
      sha256 = "sha256-TLfizJEtl1tOdVyT5E5/DeYu+SQKCaibc1SQz0cTeSw=";
    };

    phases = [ "installPhase" "patchPhase" ];

    installPhase = ''
      mkdir -p "$out/share/zellij/"
      install -Dm0644 $src "$out/share/zellij/monocle.wasm"
    '';
  };

  zellij-zjstatus = pkgs.stdenv.mkDerivation rec {
    pname = "zellij-zjstatus";
    version = "0.19.2";

    src = pkgs.fetchurl {
      url =
        "https://github.com/dj95/zjstatus/releases/download/v0.19.2/zjstatus.wasm";
      sha256 = "sha256-Jp3l3HLQxN1Jd4jyJPD7ICO/1heItMFfETDjrUsOqeI=";
    };

    phases = [ "installPhase" "patchPhase" ];

    installPhase = ''
      mkdir -p "$out/share/zellij/"
      install -Dm0644 $src "$out/share/zellij/zjstatus.wasm"
    '';
  };
in {
  fonts.fontconfig = {
    defaultFonts.monospace = [ "Fira Code" ];

    enable = true;
  };

  home = {
    inherit username;

    # https://github.com/nix-community/home-manager/issues/1341
    activation.link-apps = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      rsyncArgs="--archive --checksum --chmod=-w --copy-unsafe-links --delete"
      apps_source="$genProfilePath/home-path/Applications"
      moniker="Home Manager Trampolines"
      app_target_base="${config.home.homeDirectory}/Applications"
      app_target="$app_target_base/$moniker"
      [ -e "$app_target" ] && chmod -R 700 "$app_target" && rm -rf "$app_target"
      mkdir -p "$app_target"
      ${pkgs.rsync}/bin/rsync $rsyncArgs "$apps_source/" "$app_target"
    '';

    packages = with pkgs; [
      alfred
      aerospace
      cachix
      code-cursor
      curl
      (google-cloud-sdk.withExtraComponents [
        google-cloud-sdk.components.bq
        google-cloud-sdk.components.cbt
        google-cloud-sdk.components.gke-gcloud-auth-plugin
      ])
      gradle
      jaq
      kubectl
      nerd-fonts.fira-code
      nixfmt-classic
      obsidian
      watch
      zellij-monocle
      zellij-zjstatus
    ];

    file = {
      ".config/ghostty/config".source = ./config/ghostty;

      ".config/zellij/config.kdl".text = ''
        keybinds {
            normal {
                bind "Alt m" {
                    LaunchOrFocusPlugin "file:${
                      builtins.unsafeDiscardStringContext zellij-monocle
                    }/share/zellij/monocle.wasm" {
                        floating true
                    };
                    SwitchToMode "Normal"
                }

                bind "Alt left" { GoToPreviousTab; }
                bind "Alt right" { GoToNextTab; }
            }
        }

        show_startup_tips false
        theme "cyberpunk"
      '';

      ".config/zellij/layouts/default.kdl".text = ''
        layout {
          default_tab_template {
            children
            pane size=1 borderless=true {
              plugin location="file:${
                builtins.unsafeDiscardStringContext zellij-zjstatus
              }/share/zellij/zjstatus.wasm" {
                hide_frame_for_single_pane "true"
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
                tab_sync_indicator       "  "
                tab_floating_indicator   "󰉈 "

                datetime          "#[fg=#9399B2,bg=#181825] {format} "
                datetime_format   "%A, %d %b %Y %H:%M"
                datetime_timezone "America/Toronto"
              }
            }
          }
        }
      '';

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

    homeDirectory = "/Users/${username}";

    sessionVariablesExtra = ''
      . "${pkgs.nix}/etc/profile.d/nix.sh"
    '';

    stateVersion = "24.11";
  };

  launchd = {
    agents = {
      aerospace = mkAgent { name = "Aerospace"; };
      alfred = mkAgent { name = "Alfred"; };
    };
  };

  manual = {
    html.enable = false;
    manpages.enable = false;
    json.enable = false;
  };

  news.display = "silent";

  nix = {
    extraOptions = ''
      binary-caches-parallel-connections = 20
      connect-timeout = 10
      experimental-features = nix-command flakes
    '';

    gc = {
      automatic = true;
      options = "--delete-older-than 14d";
    };

    package = pkgs.nix;
  };

  nixpkgs.config.allowUnfree = true;

  programs = {
    aerospace = {
      enable = true;

      userSettings = {
        automatically-unhide-macos-hidden-apps = true;
        mode.main.binding = {
          cmd-shift-space = [
            "exec-and-forget open /Applications/Ghostty.app"
            "workspace 2:⚙️"
          ];
          ctrl-alt-left = "move left";
          ctrl-alt-down = "move down";
          ctrl-alt-up = "move up";
          ctrl-alt-right = "move right";
          alt-minus = "resize smart -50";
          alt-equal = "resize smart +50";
          alt-1 = "workspace 1:👨‍💻";
          alt-2 = "workspace 2:⚙️";
          alt-3 = "workspace 3:🛠️";
          alt-4 = "workspace 4:📝";
          alt-5 = "workspace 5";
          alt-6 = "workspace 6";
          alt-7 = "workspace 7";
          alt-8 = "workspace 8:🎶";
          alt-9 = "workspace 9:💬";
          alt-0 = "balance-sizes";
          alt-shift-1 = "move-node-to-workspace 1:👨‍💻";
          alt-shift-2 = "move-node-to-workspace 2:⚙️";
          alt-shift-3 = "move-node-to-workspace 3:🛠️";
          alt-shift-4 = "move-node-to-workspace 4:📝";
          alt-shift-5 = "move-node-to-workspace 5";
          alt-shift-6 = "move-node-to-workspace 6";
          alt-shift-7 = "move-node-to-workspace 7";
          alt-shift-8 = "move-node-to-workspace 8:🎶";
          alt-shift-9 = "move-node-to-workspace 9:💬";
          alt-shift-0 = "balance-sizes";
          alt-tab = "workspace-back-and-forth";
        };
        on-window-detected = [
          {
            "if".app-id = "com.google.Chrome";
            check-further-callbacks = false;
            run = [ "move-node-to-workspace 1:👨‍💻" ];
          }
          {
            "if".app-id = "org.mozilla.firefox";
            check-further-callbacks = false;
            run = [ "move-node-to-workspace 1:👨‍💻" ];
          }
          {
            "if".app-id = "com.apple.Safari";
            check-further-callbacks = false;
            run = [ "move-node-to-workspace 1:👨‍💻" ];
          }
          {
            "if".app-id = "com.mitchellh.ghostty";
            check-further-callbacks = false;
            run = [ "move-node-to-workspace 2:⚙️" ];
          }
          {
            "if".app-id = "com.microsoft.VSCode";
            check-further-callbacks = false;
            run = [ "move-node-to-workspace 3:🛠️" ];
          }
          {
            "if".app-id = "com.jetbrains.intellij";
            check-further-callbacks = false;
            run = [ "move-node-to-workspace 3:🛠️" ];
          }
          {
            "if".app-id = "md.obsidian";
            check-further-callbacks = false;
            run = [ "move-node-to-workspace 4:📝" ];
          }
          {
            "if".app-id = "com.tidal.desktop";
            check-further-callbacks = false;
            run = [ "move-node-to-workspace 8:🎶" ];
          }
          {
            "if".app-id = "com.tinyspeck.slackmacgap";
            check-further-callbacks = false;
            run = [ "move-node-to-workspace 9:💬" ];
          }
        ];
        start-at-login = true;
      };
    };

    bat.enable = true;

    eza = {
      enable = true;
      icons = "auto";
      git = true;
    };

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
        credential.helper = "osxkeychain";
        diff.algorithm = "patience";
        diff.colorMoved = "default";
        gc.writeCommitGraph = "true";
        help.autocorrect = "5";
        init.defaultBranch = "main";
        merge.renamelimit = "4096";
        pull.ff = "only";
        pull.rebase = "true";
        push.autoSetupRemote = "true";
        push.default = "upstream";
        rebase.updateRefs = "true";
        submodule.recurse = "true";
        tag.sort = "version:refname";
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
        ".idea"
        ".intellij"
        "gradle-wrapper.properties"
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
        key = gpgKey;
        signByDefault = true;
        signer = "/opt/dev/bin/gpg-auto-pin";
      };

      userEmail = email;
      userName = name;
    };

    htop = {
      enable = true;
      settings.show_program_path = true;
    };

    home-manager.enable = true;

    jq.enable = true;

    ripgrep.enable = true;

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

      package = pkgs.code-cursor;

      profiles.default = {
        enableUpdateCheck = false;

        extensions = with pkgs.vscode-extensions;
          [
            bbenoist.nix
            brettm12345.nixfmt-vscode
            eamodio.gitlens
            hashicorp.terraform
            mathiasfrohlich.kotlin
            ms-azuretools.vscode-docker
            ms-python.python
            redhat.java
            redhat.vscode-yaml
            scala-lang.scala
            scalameta.metals
            tamasfe.even-better-toml
            vscjava.vscode-gradle
            vscjava.vscode-java-debug
            vscjava.vscode-java-dependency
            vscjava.vscode-java-pack
            vscjava.vscode-java-test
            zxh404.vscode-proto3
          ]
          ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [ vscode_nord ];

        userSettings = {
          "editor.formatOnSave" = true;
          "editor.wordWrap" = "on";
          "files.associations" = { "*.ejson" = "json"; };
          "files.insertFinalNewline" = true;
          "files.trimTrailingWhitespace" = true;
          "files.watcherExclude" = {
            "**/.bloop" = true;
            "**/.metals" = true;
            "**/.ammonite" = true;
          };
          "editor.fontFamily" = "Fira Code";
          "editor.fontLigatures" = true;
          "editor.fontSize" = 12;
          "gitlens.launchpad.indicator.enabled" = false;
          "gitlens.statusBar.enabled" = false;
          "gitlens.telemetry.enabled" = false;
          "gradle.nestedProjects" = true;
          "java.debug.settings.vmArgs" = "-Xmx4G";
          "java.test.config" = [{
            "name" = "Java 17";
            "vmArgs" = [
              "-Xmx12G"
              "-Xms1G"
              "--add-opens=java.base/java.lang=ALL-UNNAMED"
              "--add-opens=java.base/java.util=ALL-UNNAMED"
            ];
          }];
          "metals.enableIndentOnPaste" = true;
          "metals.serverVersion" = "1.5.3";
          "redhat.telemetry.enabled" = false;
          "telemetry.telemetryLevel" = "off";
          "workbench.colorTheme" = "Nord";
          "workbench.preferredDarkColorTheme" = "Nord";
          "workbench.preferredLightColorTheme" = "Nord";
          "workbench.startupEditor" = "none";
        };
      };

      mutableExtensionsDir = false;
    };

    zellij = {
      # https://github.com/nix-community/home-manager/issues/5017
      attachExistingSession = false;
      enable = true;
      enableZshIntegration = false;
      exitShellOnExit = false;
    };

    zoxide = {
      enable = true;
      enableZshIntegration = true;
      options = [ "--cmd" "cd" ];
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

        function fixkube() {
          [ -e ${config.home.homeDirectory}/.kube/config.shopify.cloudplatform ] && (grep -q "nix/profiles/home-manager/home-path/bin/kubectl" ${config.home.homeDirectory}/.kube/config.shopify.cloudplatform || ${pkgs.gnused}/bin/sed -i 's|command: gke-gcloud-auth-plugin|command: ${config.home.homeDirectory}/.local/state/nix/profiles/home-manager/home-path/bin/gke-gcloud-auth-plugin|g' ${config.home.homeDirectory}/.kube/config.shopify.cloudplatform)
        }

        function update() {
          nix-channel --update && home-manager switch -b backup && brew update && brew upgrade && clear
        }

        # Tooling
        [ -f /opt/dev/dev.sh ] && source /opt/dev/dev.sh
        [[ -x /usr/local/bin/brew ]] && eval $(/usr/local/bin/brew shellenv)
        [[ -x /opt/homebrew/bin/brew ]] && eval $(/opt/homebrew/bin/brew shellenv)
        [[ -f /opt/dev/sh/chruby/chruby.sh ]] && { type chruby >/dev/null 2>&1 || chruby () { source /opt/dev/sh/chruby/chruby.sh; chruby "$@"; } }

        # Kubernetes
        # cloudplatform: add Shopify clusters to your local kubernetes config
        if [ -d "${config.home.homeDirectory}/src/github.com/Shopify/cloudplatform" ]; then
          export KUBECONFIG=''${KUBECONFIG:+$KUBECONFIG:}${config.home.homeDirectory}/.kube/config:${config.home.homeDirectory}/.kube/config.shopify.cloudplatform
          fixkube
          for file in ${config.home.homeDirectory}/src/github.com/Shopify/cloudplatform/workflow-utils/*.bash; do source ''${file}; done
          kubectl-short-aliases
        fi

        # https://gist.github.com/JonnieCache/1e2fdc2f5737f640e150ea40da5b9d1d
        function current_dir() {
            local current_dir=$PWD
            if [[ $current_dir == $HOME ]]; then
                current_dir="~"
            else
                current_dir=''${current_dir##*/}
            fi

            echo $current_dir
        }

        function change_tab_title() {
            local title=$1
            command nohup ${pkgs.zellij}/bin/zellij action rename-tab $title >/dev/null 2>&1
        }

        function set_tab_to_working_dir() {
            local result=$?
            local title=$(current_dir)
            # uncomment the following to show the exit code after a failed command
            # if [[ $result -gt 0 ]]; then
            #     title="$title [$result]"
            # fi

            change_tab_title $title
        }

        function set_tab_to_command_line() {
            local cmdline=$1
            change_tab_title $cmdline
        }

        if [[ -n $ZELLIJ ]]; then
            add-zsh-hook precmd set_tab_to_working_dir
            add-zsh-hook preexec set_tab_to_command_line
        fi

        # https://github.com/nix-community/home-manager/issues/5017
        if [[ -z "$ZELLIJ" ]]; then
            if [[ "$ZELLIJ_AUTO_ATTACH" == "true" ]]; then
                ${pkgs.zellij}/bin/zellij attach -c
            else
                ${pkgs.zellij}/bin/zellij
            fi

            if [[ "$ZELLIJ_AUTO_EXIT" == "true" ]]; then
                exit
            fi
        fi
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
          "/nix/var/nix/profiles/system/sw/bin:${config.home.homeDirectory}/.local/state/nix/profiles/home-manager/home-path/bin:${config.home.homeDirectory}/.nix-profile/bin:/nix/var/nix/profiles/default/bin:${config.home.homeDirectory}/.local/bin:$PATH";
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
