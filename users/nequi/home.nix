{ pkgs, config, ... }:
let
  sensible-defaults = pkgs.fetchurl {
    url =
      "https://raw.githubusercontent.com/hrs/sensible-defaults.el/d9001f0efc9382f8587357f2670fc4d8594858af/sensible-defaults.el";
    sha256 = "09l03619bchh6dh0fnanvmwp50pmnv4x8c8qqgyv4kmwd553ba9n";
  };
in {
  home = {
    file = {
      ".conky".source = ./conkyrc;
      ".emacs.d/config.org".source = ./config.org;
      ".emacs.d/init.el".source = ./init.el;
      ".emacs.d/sensible-defaults.el".source = sensible-defaults;
      ".config/nixpkgs/config.nix".source = ./config.nix;
      ".local/bin/nix-updates".source = ./nix-updates.sh;

      ".nanorc".text = ''
        set linenumbers
        set tabsize 2
        set tabstospaces
        set trimblanks
        set unix

        include ${pkgs.nanorc}/share/*.nanorc
      '';

      ".xinitrc".text = ''
        xrdb ~/.Xresources
        [[ -f ~/.Xdefaults ]] && xrdb -merge ~/.Xdefaults
        nix-shell -p xorg.xmodmap --command "xmodmap ~/.Xmodmap"
      '';

      ".xmobar/config".text = ''
        Config { font = "xft:DejaVu Sans Mono:size=8:antialias=true,Font Awesome 5 Free Solid:size=9,Font Awesome 5 Brands Regular:size=9"
           , additionalFonts = []
           , borderColor = "black"
           , border = BottomB
           , bgColor = "black"
           , fgColor = "grey"
           , alpha = 255
           , position = Bottom
           , textOffset = -1
           , iconOffset = -1
           , lowerOnStart = False
           , pickBroadest = False
           , persistent = False
           , hideOnStart = False
           , iconRoot = "."
           , allDesktops = True
           , overrideRedirect = False
           , commands = [ Run DynNetwork [ "--template" , "<dev>: <rx>kB/s | <tx>kB/s" ] 10
                        , Run MultiCpu [ "--template" , "Cpu: <autobar>" ] 10
                        , Run Memory ["-t","Mem: <used>"] 10
                        , Run Com "uname" ["-r"] "" 36000
                        , Run Date "%A, %d.%m.%Y %H:%M:%S" "date" 10
                        , Run Battery        [ "--template" , "<acstatus>"
                                 , "--Low"      , "10"        -- units: %
                                 , "--High"     , "80"        -- units: %
                                 , "--low"      , "darkred"
                                 , "--normal"   , "darkorange"
                                 , "--high"     , "darkgreen"

                                 , "--" -- battery specific options
                                           -- discharging status
                                           , "-o"  , "<left>% (<timeleft>)"
                                           -- AC "on" status
                                           , "-O"  , "<fc=#dAA520>Charging</fc>"
                                           -- charged status
                                           , "-i"  , "<fc=#006000>Charged</fc>"
                                 ] 50
                        , Run StdinReader
                        ]
           , sepChar = "%"
           , alignSep = "}{"
           , template = "%StdinReader% }{  %multicpu% |  %memory% MiB |  %dynnetwork% |  %date% |  %battery% |  %uname%"
           }

      '';

      ".Xmodmap".text = ''
        keycode 66 = Mode_switch Multi_key
        keycode 39 = s S ssharp
        keycode 38 = a A adiaeresis Adiaeresis
        keycode 30 = u U udiaeresis Udiaeresis
        keycode 32 = o O odiaeresis Odiaeresis
      '';

      ".Xresources".source = ./Xresources;
    };

    packages = with pkgs; [ nano ];
  };

  programs = {
    alacritty = {
      enable = true;

      settings = {
        alt_send_esc = true;
        background_opacity = 1.0;

        bell = {
          animation = "EaseOutExpo";
          color = "0xffffff";
          duration = 0;
        };

        colors = {
          bright = {
            black = "0x666666";
            blue = "0x81a2be";
            cyan = "0x54ced6";
            green = "0x9ec400";
            magenta = "0xb77ee0";
            red = "0xff3334";
            white = "0x282a2e";
            yellow = "0xf0c674";
          };

          cursor = {
            cursor = "0xffffff";
            text = "0x1d1f21";
          };

          indexed_colors = [ ];

          normal = {
            black = "0x1d1f21";
            blue = "0x81a2be";
            cyan = "0x70c0ba";
            green = "0xb5bd68";
            magenta = "0xb294bb";
            red = "0xcc6666";
            white = "0x373b41";
            yellow = "0xe6c547";
          };

          primary = {
            background = "0x1d1f21";
            foreground = "0xc5c8c6";
          };
        };

        cursor = {
          style = "Block";
          unfocused_hollow = true;
        };

        debug = {
          log_level = "Warn";
          persistent_logging = false;
          print_events = false;
          ref_test = false;
          render_time = false;
        };

        draw_bold_text_with_bright_colors = true;
        enable_experimental_conpty_backend = false;
        env.TERM = "xterm-256color";

        font = {
          glyph_offset = {
            x = 0;
            y = 0;
          };
          offset = {
            x = 0;
            y = 0;
          };
          size = 11;
          use_thin_strokes = false;
        };

        live_config_reload = true;

        key_bindings = [
          {
            key = "Paste";
            action = "Paste";
          }
          {
            key = "Copy";
            action = "Copy";
          }
          {
            key = "L";
            mods = "Control";
            action = "ClearLogNotice";
          }
          {
            key = "L";
            mods = "Control";
            chars = "\\x0c";
          }
          {
            key = "Home";
            mods = "Alt";
            chars = "\\x1b[1;3H";
          }
          {
            key = "Home";
            chars = "\\x1bOH";
            mode = "AppCursor";
          }
          {
            key = "Home";
            chars = "\\x1b[H";
            mode = "~AppCursor";
          }
          {
            key = "End";
            mods = "Alt";
            chars = "\\x1b[1;3F";
          }
          {
            key = "End";
            chars = "\\x1bOF";
            mode = "AppCursor";
          }
          {
            key = "End";
            chars = "\\x1b[F";
            mode = "~AppCursor";
          }
          {
            key = "PageUp";
            mods = "Shift";
            action = "ScrollPageUp";
            mode = "~Alt";
          }
          {
            key = "PageUp";
            mods = "Shift";
            chars = "\\x1b[5;2~";
            mode = "Alt";
          }
          {
            key = "PageUp";
            mods = "Control";
            chars = "\\x1b[5;5~";
          }
          {
            key = "PageUp";
            mods = "Alt";
            chars = "\\x1b[5;3~";
          }
          {
            key = "PageUp";
            chars = "\\x1b[5~";
          }
          {
            key = "PageDown";
            mods = "Shift";
            action = "ScrollPageDown";
            mode = "~Alt";
          }
          {
            key = "PageDown";
            mods = "Shift";
            chars = "\\x1b[6;2~";
            mode = "Alt";
          }
          {
            key = "PageDown";
            mods = "Control";
            chars = "\\x1b[6;5~";
          }
          {
            key = "PageDown";
            mods = "Alt";
            chars = "\\x1b[6;3~";
          }
          {
            key = "PageDown";
            chars = "\\x1b[6~";
          }
          {
            key = "Tab";
            mods = "Shift";
            chars = "\\x1b[Z";
          }
          {
            key = "Back";
            chars = "\\x7f";
          }
          {
            key = "Back";
            mods = "Alt";
            chars = "\\x1b\\x7f";
          }
          {
            key = "Insert";
            chars = "\\x1b[2~";
          }
          {
            key = "Delete";
            chars = "\\x1b[3~";
          }
          {
            key = "Left";
            mods = "Shift";
            chars = "\\x1b[1;2D";
          }
          {
            key = "Left";
            mods = "Control";
            chars = "\\x1b[1;5D";
          }
          {
            key = "Left";
            mods = "Alt";
            chars = "\\x1b[1;3D";
          }
          {
            key = "Left";
            chars = "\\x1b[D";
            mode = "~AppCursor";
          }
          {
            key = "Left";
            chars = "\\x1bOD";
            mode = "AppCursor";
          }
          {
            key = "Right";
            mods = "Shift";
            chars = "\\x1b[1;2C";
          }
          {
            key = "Right";
            mods = "Control";
            chars = "\\x1b[1;5C";
          }
          {
            key = "Right";
            mods = "Alt";
            chars = "\\x1b[1;3C";
          }
          {
            key = "Right";
            chars = "\\x1b[C";
            mode = "~AppCursor";
          }
          {
            key = "Right";
            chars = "\\x1bOC";
            mode = "AppCursor";
          }
          {
            key = "Up";
            mods = "Shift";
            chars = "\\x1b[1;2A";
          }
          {
            key = "Up";
            mods = "Control";
            chars = "\\x1b[1;5A";
          }
          {
            key = "Up";
            mods = "Alt";
            chars = "\\x1b[1;3A";
          }
          {
            key = "Up";
            chars = "\\x1b[A";
            mode = "~AppCursor";
          }
          {
            key = "Up";
            chars = "\\x1bOA";
            mode = "AppCursor";
          }
          {
            key = "Down";
            mods = "Shift";
            chars = "\\x1b[1;2B";
          }
          {
            key = "Down";
            mods = "Control";
            chars = "\\x1b[1;5B";
          }
          {
            key = "Down";
            mods = "Alt";
            chars = "\\x1b[1;3B";
          }
          {
            key = "Down";
            chars = "\\x1b[B";
            mode = "~AppCursor";
          }
          {
            key = "Down";
            chars = "\\x1bOB";
            mode = "AppCursor";
          }
          {
            key = "F1";
            chars = "\\x1bOP";
          }
          {
            key = "F2";
            chars = "\\x1bOQ";
          }
          {
            key = "F3";
            chars = "\\x1bOR";
          }
          {
            key = "F4";
            chars = "\\x1bOS";
          }
          {
            key = "F5";
            chars = "\\x1b[15~";
          }
          {
            key = "F6";
            chars = "\\x1b[17~";
          }
          {
            key = "F7";
            chars = "\\x1b[18~";
          }
          {
            key = "F8";
            chars = "\\x1b[19~";
          }
          {
            key = "F9";
            chars = "\\x1b[20~";
          }
          {
            key = "F10";
            chars = "\\x1b[21~";
          }
          {
            key = "F11";
            chars = "\\x1b[23~";
          }
          {
            key = "F12";
            chars = "\\x1b[24~";
          }
          {
            key = "F1";
            mods = "Shift";
            chars = "\\x1b[1;2P";
          }
          {
            key = "F2";
            mods = "Shift";
            chars = "\\x1b[1;2Q";
          }
          {
            key = "F3";
            mods = "Shift";
            chars = "\\x1b[1;2R";
          }
          {
            key = "F4";
            mods = "Shift";
            chars = "\\x1b[1;2S";
          }
          {
            key = "F5";
            mods = "Shift";
            chars = "\\x1b[15;2~";
          }
          {
            key = "F6";
            mods = "Shift";
            chars = "\\x1b[17;2~";
          }
          {
            key = "F7";
            mods = "Shift";
            chars = "\\x1b[18;2~";
          }
          {
            key = "F8";
            mods = "Shift";
            chars = "\\x1b[19;2~";
          }
          {
            key = "F9";
            mods = "Shift";
            chars = "\\x1b[20;2~";
          }
          {
            key = "F10";
            mods = "Shift";
            chars = "\\x1b[21;2~";
          }
          {
            key = "F11";
            mods = "Shift";
            chars = "\\x1b[23;2~";
          }
          {
            key = "F12";
            mods = "Shift";
            chars = "\\x1b[24;2~";
          }
          {
            key = "F1";
            mods = "Control";
            chars = "\\x1b[1;5P";
          }
          {
            key = "F2";
            mods = "Control";
            chars = "\\x1b[1;5Q";
          }
          {
            key = "F3";
            mods = "Control";
            chars = "\\x1b[1;5R";
          }
          {
            key = "F4";
            mods = "Control";
            chars = "\\x1b[1;5S";
          }
          {
            key = "F5";
            mods = "Control";
            chars = "\\x1b[15;5~";
          }
          {
            key = "F6";
            mods = "Control";
            chars = "\\x1b[17;5~";
          }
          {
            key = "F7";
            mods = "Control";
            chars = "\\x1b[18;5~";
          }
          {
            key = "F8";
            mods = "Control";
            chars = "\\x1b[19;5~";
          }
          {
            key = "F9";
            mods = "Control";
            chars = "\\x1b[20;5~";
          }
          {
            key = "F10";
            mods = "Control";
            chars = "\\x1b[21;5~";
          }
          {
            key = "F11";
            mods = "Control";
            chars = "\\x1b[23;5~";
          }
          {
            key = "F12";
            mods = "Control";
            chars = "\\x1b[24;5~";
          }
          {
            key = "F1";
            mods = "Alt";
            chars = "\\x1b[1;6P";
          }
          {
            key = "F2";
            mods = "Alt";
            chars = "\\x1b[1;6Q";
          }
          {
            key = "F3";
            mods = "Alt";
            chars = "\\x1b[1;6R";
          }
          {
            key = "F4";
            mods = "Alt";
            chars = "\\x1b[1;6S";
          }
          {
            key = "F5";
            mods = "Alt";
            chars = "\\x1b[15;6~";
          }
          {
            key = "F6";
            mods = "Alt";
            chars = "\\x1b[17;6~";
          }
          {
            key = "F7";
            mods = "Alt";
            chars = "\\x1b[18;6~";
          }
          {
            key = "F8";
            mods = "Alt";
            chars = "\\x1b[19;6~";
          }
          {
            key = "F9";
            mods = "Alt";
            chars = "\\x1b[20;6~";
          }
          {
            key = "F10";
            mods = "Alt";
            chars = "\\x1b[21;6~";
          }
          {
            key = "F11";
            mods = "Alt";
            chars = "\\x1b[23;6~";
          }
          {
            key = "F12";
            mods = "Alt";
            chars = "\\x1b[24;6~";
          }
          {
            key = "F1";
            mods = "Super";
            chars = "\\x1b[1;3P";
          }
          {
            key = "F2";
            mods = "Super";
            chars = "\\x1b[1;3Q";
          }
          {
            key = "F3";
            mods = "Super";
            chars = "\\x1b[1;3R";
          }
          {
            key = "F4";
            mods = "Super";
            chars = "\\x1b[1;3S";
          }
          {
            key = "F5";
            mods = "Super";
            chars = "\\x1b[15;3~";
          }
          {
            key = "F6";
            mods = "Super";
            chars = "\\x1b[17;3~";
          }
          {
            key = "F7";
            mods = "Super";
            chars = "\\x1b[18;3~";
          }
          {
            key = "F8";
            mods = "Super";
            chars = "\\x1b[19;3~";
          }
          {
            key = "F9";
            mods = "Super";
            chars = "\\x1b[20;3~";
          }
          {
            key = "F10";
            mods = "Super";
            chars = "\\x1b[21;3~";
          }
          {
            key = "F11";
            mods = "Super";
            chars = "\\x1b[23;3~";
          }
          {
            key = "F12";
            mods = "Super";
            chars = "\\x1b[24;3~";
          }
          {
            key = "NumpadEnter";
            chars = "\\n";
          }

        ];

        mouse = {
          double_click.threshold = 300;
          hide_when_typing = false;
          triple_click.threshold = 300;
          url.modifiers = "None";
        };

        mouse_bindings = [{
          action = "PasteSelection";
          mouse = "Middle";
        }];

        scrolling = {
          history = 10000;
          multiplier = 3;
        };

        selection = {
          save_to_clipboard = false;
          semantic_escape_chars = ''",│`|:"' ()[]{}<>"'';
        };

        window = {
          decorations = "full";
          dimensions = {
            columns = 0;
            lines = 0;
          };
          dynamic_padding = false;
          dynamic_title = true;
          padding = {
            x = 0;
            y = 0;
          };
          startup_mode = "Windowed";
        };

        working_directory = "None";
      };

    };

    emacs = {
      enable = true;
      extraPackages = epkgs:
        (with epkgs.melpaPackages; [
          all-the-icons
          ample-theme
          auto-complete
          company-lsp
          dockerfile-mode
          company
          editorconfig
          flycheck
          forge
          groovy-mode
          haskell-mode
          hasklig-mode
          hl-todo
          htmlize
          indent-guide
          ivy
          json-mode
          lsp-mode
          lsp-ui
          magit
          markdown-mode
          multi-term
          multiple-cursors
          nix-mode
          nyan-mode
          ox-gfm
          projectile
          rainbow-delimiters
          rust-mode
          sbt-mode
          scala-mode
          smartparens
          smooth-scrolling
          treemacs
          treemacs-magit
          treemacs-icons-dired
          treemacs-projectile
          use-package
          yaml-mode
          yasnippet
        ]) ++ (with epkgs.elpaPackages; [ beacon ]);
    };

    gpg.enable = true;

    git = {
      aliases = {
        clear = "clean -dfx";
        lg =
          "log --all --decorate --color --graph --pretty=format:'%Cred%h%Creset %Cgreen(%cr)%Creset - %s %C(bold blue)<%an>[%G?]%Creset%C(auto)%d%Creset' --abbrev-commit";
        ll = ''
          log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate --numstat'';
        undo = "reset HEAD~1 --mixed";
      };

      delta.enable = true;

      enable = true;

      extraConfig = {
        apply.whitespace = "fix";
        branch.autosetuprebase = "always";
        core.whitespace = "fix,-indent-with-non-tab,trailing-space,cr-at-eol";
        init.defaultBranch = "main";
        merge.renamelimit = "4096";
        pull.rebase = "true";
        push.default = "upstream";
        submodule.recurse = "true";
        tag.sort = "version:refname";
      };

      ignores = [
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
        key = "ACD70987F33D77B8A956E89FA8AECBD02786E3F0";
        signByDefault = true;
      };

      userEmail = "tim@nequissimus.com";
      userName = "Tim Steinbach";
    };

    htop.enable = true;

    home-manager.enable = true;

    jq.enable = true;

    rofi = {
      enable = true;
      theme = "${pkgs.rofi}/share/rofi/themes/sidebar.rasi";
    };

    ssh = {
      enable = true;

      extraConfig = ''
        ChallengeResponseAuthentication no
        Ciphers chacha20-poly1305@openssh.com,aes256-gcm@openssh.com,aes128-gcm@openssh.com,aes256-ctr,aes192-ctr,aes128-ctr
        ConnectTimeout 60
        ConnectionAttempts 60
        HostKeyAlgorithms ssh-ed25519-cert-v01@openssh.com,ssh-rsa-cert-v01@openssh.com,ssh-ed25519,ssh-rsa
        KexAlgorithms curve25519-sha256@libssh.org,diffie-hellman-group-exchange-sha256
        MACs hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com,umac-128-etm@openssh.com,hmac-sha2-512,hmac-sha2-256,umac-128@openssh.com
        PasswordAuthentication no
        PubkeyAuthentication yes
        UseRoaming no
      '';

      hashKnownHosts = true;
      serverAliveInterval = 240;
    };

    zsh = {
      enable = true;
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
        JAVA_HOME = "${pkgs.openjdk8}";
        PATH = "${config.home.homeDirectory}/.local/bin:$PATH";
        TERMINAL = "xterm";
        XZ_DEFAULTS = "-T 0";
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
  };

  xsession.windowManager.xmonad = {
    enable = true;

    extraPackages = haskellPackages: [ haskellPackages.xmonad-contrib ];

    config = pkgs.writeText "xmonad.hs" ''
      import Control.Monad (liftM2)
      import XMonad
      import XMonad.Actions.CycleWS
      import XMonad.Hooks.DynamicLog
      import XMonad.Hooks.ManageDocks
      import XMonad.Hooks.ManageHelpers
      import XMonad.Hooks.EwmhDesktops
      import XMonad.Layout.NoBorders
      import XMonad.Util.Run(spawnPipe)
      import XMonad.Util.EZConfig(additionalKeys)
      import System.IO
      import qualified Data.Map as M
      import qualified XMonad.StackSet as W
      --  = 61728
      --  = 61729
      --  = 62057
      --  = 61848
      myWorkspaces = [ "\61728", "\61729", "3", "4", "5", "6", "7", "8", "\62057", "\61848" ]
      myManageHook = composeAll [
          isFullscreen --> (doF W.focusDown <+> doFullFloat)
          , isDialog --> doFloat
          , className =? "Chromium-browser" --> viewShift "\62057"
          , className =? "Firefox" --> viewShift "\62057"
          , className =? "Vivaldi-stable" --> viewShift "\62057"
          , className =? "Emacs" --> viewShift "\61729"
          , className =? "Slack" --> viewShift "\61848"
          , appName =? "desktop_window" --> doIgnore
          ]
          where viewShift = doF . liftM2 (.) W.greedyView W.shift
      myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $ [
        ((modm, xK_Return), spawn $ XMonad.terminal conf),
        ((modm .|. shiftMask, xK_l), spawn "slock"),
        ((modm, xK_d), spawn "rofi -show run"),
        ((modm, xK_w), kill),
        ((modm, xK_Left), windows W.focusUp),
        ((modm, xK_Right), windows W.focusDown),
        ((modm .|. shiftMask, xK_Left), windows W.swapUp),
        ((modm .|. shiftMask, xK_Right), windows W.swapDown)
        ]
        ++ [((m .|. modm, k), windows $ f i)
           | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [ xK_0 ])
           , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
        ++ [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
           | (key, sc) <- zip [xK_q, xK_w, xK_e] [0, 2, 1]
           , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
      myPP = xmobarPP {
          ppOutput          = putStrLn
        , ppCurrent         = xmobarColor "#EEEE00" "" . wrap " " " "
        , ppTitle           = xmobarColor "yellow"  "" . shorten 20
        , ppLayout          = shorten 6
        , ppUrgent          = xmobarColor "red" "yellow"
        }
      main = do
        xmproc <- spawnPipe "xmobar ${config.home.homeDirectory}/.xmobar/config"
        xmonad $ def {
          focusedBorderColor = "#F0F0F0",
          handleEventHook = docksEventHook <+> handleEventHook def,
          keys = myKeys,
          layoutHook = avoidStruts $ layoutHook def,
          logHook = dynamicLogWithPP myPP
                          { ppOrder = \(ws:l:t:_) -> ws : [t]
                          , ppOutput = hPutStrLn xmproc
                          , ppTitle = xmobarColor "green" "" . shorten 50 },
          manageHook = myManageHook <+> manageDocks <+> manageHook def,
          modMask = mod4Mask,
          normalBorderColor = "#444444",
          terminal = "alacritty",
          workspaces = myWorkspaces
        }
    '';

  };
}
