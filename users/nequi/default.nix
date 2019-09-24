with import <nixpkgs> {};
with import <nixhome> { inherit stdenv; inherit pkgs; };
with lib;
let
  user = "nequi";
  checkout = "/home/${user}/dev/DevSetup";
  base = "${checkout}/users/${user}";
in mkHome {
  inherit user;

  files = {
    # Rofi
    ".config/rofi/config".content = "rofi.theme: ${pkgs.rofi}/share/rofi/themes/sidebar.rasi";

    # Ammonite
    ".ammonite/predef.sc".content = "interp.repositories() ++= Seq(coursier.maven.MavenRepository(\"https://oss.sonatype.org/content/repositories/releases\"))";

    # Conky
    ".conky".content = lib.fileContents "${base}/conkyrc";

    # Nix
    ".config/nixpkgs/config.nix".content = "{ allowUnfree = true; }";

    # Emacs
    ".emacs.d/init.el".content = lib.fileContents "${base}/init.el";

    # Git
    ".gitconfig".content = lib.fileContents "${base}/gitconfig";
    ".gitignore".content = lib.fileContents "${base}/gitignore";

    #GnuPG
    ".gnupg/gpg.conf".content = lib.fileContents "${base}/gpg.conf";

    # Nano
    ".nanorc".content = ''
      set linenumbers
      set tabsize 2
      set tabstospaces
      set trimblanks
      set unix

      include ${pkgs.nanorc}/share/*.nanorc
    '';

    # X
    ".xinitrc".content = ''
      xrdb ~/.Xresources
      [[ -f ~/.Xdefaults ]] && xrdb -merge ~/.Xdefaults
      nix-shell -p xorg.xmodmap --command "xmodmap ~/.Xmodmap"
    '';
    ".Xmodmap".content = ''
      keycode 66 = Mode_switch Multi_key
      keycode 39 = s S ssharp
      keycode 38 = a A adiaeresis Adiaeresis
      keycode 30 = u U udiaeresis Udiaeresis
      keycode 32 = o O odiaeresis Odiaeresis
    '';
    ".Xresources".content = lib.fileContents "${base}/Xresources";

    # XMonad
    ".xmonad/xmonad.hs".content = ''
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
        xmproc <- spawnPipe "xmobar /home/${user}/.xmobar/config"
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
          terminal = "urxvtc",
          workspaces = myWorkspaces
        }
    '';

    ".xmobar/config".content = ''
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

    # ZSH
    ".zshrc".content = ''
      #!/usr/env/bin zsh

      # oh-my-zsh
      export ZSH_THEME="spaceship"
      export SPACESHIP_CHAR_SYMBOL="λ "
      export SPACESHIP_PROMPT_SEPARATE_LINE=false
      export SPACESHIP_PROMPT_ORDER=(dir git rust haskell docker exec_time battery jobs exit_code char)
      export CASE_SENSITIVE="false"
      export GPG_TTY="$(tty)"
      export HIST_STAMPS="dd.mm.yyyy"
      export HISTCONTROL="ignoredups"
      export plugins=(docker emacs git github gitignore kubectl postgres sbt scala ssh-agent)

      # Configure ssh-agent
      zstyle :omz:plugins:ssh-agent agent-forwarding on
      zstyle :omz:plugins:ssh-agent lifetime 1h

      # ENV
      export PATH="''${HOME}/.local/bin:''${PATH}"
      export TERMINAL="xterm"
      export TERM="linux"
      export JAVA_HOME="''${$(readlink -e $(type -p java))%*/bin/java}"

      # ZSH for non-NixOS
      ZSH="''${ZSH:-/nix/var/nix/profiles/per-user/${user}/profile/share/oh-my-zsh}"

      # Load oh-my-zsh
      [[ -e "/home/${user}/.nix-profile/etc/profile.d/nix.sh" ]] && source /home/${user}/.nix-profile/etc/profile.d/nix.sh
      [[ -e "''${ZSH}/oh-my-zsh.sh" ]] && source "''${ZSH}/oh-my-zsh.sh"

      # Fix ZSH
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

      # Aliases
      alias cat='bat'
      alias diff='diff --color'
      alias git='hub'
      alias grep='rg'
      alias ls='exa'
      alias mvn='mvn -q'
      alias nano='nano -E -w -c'
      alias nix-repair-store='nix-store --verify --check-contents --repair'
      alias rsync='rsync -azvvP'
      alias scrot='scrot -s'
      alias volume_down='amixer -q sset Master 5%-; volume'
      alias volume_up='amixer -q sset Master 5%+; volume'
      alias volume='awk -F"[][]" "/dB/ { print $2 }" <(amixer sget Master)'

      # Docker tools
      function docker_clean() { docker kill $(docker ps -q); docker rm $(docker ps -a -q); }
      function docker_clean_dangling() { docker images -qf dangling=true | xargs -r docker rmi; }
      function docker_clean_images() { docker kill $(docker ps -q); docker rm $(docker ps -a -q); docker rmi -f $(docker images -q); }
      function docker_inspect() { (skopeo inspect docker://"$1" || docker inspect "$1") | jq; }

      # Nix updates
      function nix-updates() {
      # https://github.com/NixOS/nixpkgs/pull/57800
      # ./pkgs/applications/audio/spotify/update.sh \

      cd "''${HOME}/dev/upstream_nix/" \
        && git checkout master \
        && git pull \
        && ./pkgs/os-specific/linux/kernel/update.sh && (nix-build -A linux_4_4.configfile -A linux_4_9.configfile -A linux_4_14.configfile -A linux_4_19.configfile -A linux_latest.configfile -A linux_hardened.configfile -A linux_latest_hardened.configfile -A linux_testing.configfile || (git reset --hard origin/master && git checkout -- .)) \
        && ./pkgs/applications/networking/browsers/vivaldi/update.sh && (nix-build -A vivaldi || (git reset --mixed HEAD~2 && git checkout -- .)) && (nix-build -A vivaldi-ffmpeg-codecs || (git reset --mixed HEAD~1 && git checkout -- .))  \
        && ./pkgs/applications/networking/instant-messengers/zoom-us/update.sh && (nix-build -A zoom-us || (git reset --mixed HEAD~1 && git checkout -- .)) \
        && ./pkgs/development/tools/continuous-integration/jenkins/update.sh && (nix-build -A jenkins || (git reset --mixed HEAD~1 && git checkout -- .)) \
        && ./pkgs/applications/version-management/git-and-tools/git/update.sh && (nix-build -A git || (git reset --mixed HEAD~1 && git checkout -- .)) \
        && ./pkgs/shells/zsh/oh-my-zsh/update.sh && (nix-build -A oh-my-zsh || (git reset --mixed HEAD~1 && git checkout -- .)) \
        && ./pkgs/applications/networking/instant-messengers/slack/update.sh && (nix-build -A slack-theme-black || (git reset --mixed HEAD~1 && git checkout -- .))
      }

      # Nix review PRs
      function noxpr() { nix-shell -p nox --run "nox-review pr $1"; }

      # Tools
      function sbt() {
        args="$@"
        if [ -f "''${HOME}/.sbt/repositories" ]; then
          repo="-Dsbt.override.build.repos=true"
        fi
        nix-shell -p openjdk8 -p sbt-extras --command "sbt -J-Xms1G -J-Xmx8G ''${repo} ''${args}";
      }
      function amm() { nix-shell -p ammonite --command "amm"; }

      [ -f "''${HOME}/secrets.env" ] && source ''${HOME}/secrets.env
    '';
  };
}
