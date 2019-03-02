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

    # Firefox
    ".mozilla/firefox/profiles.ini".content = lib.fileContents "${base}/mozilla/firefox/profiles.init";
    ".mozilla/firefox/7ty7knlr.default/user.js".content = lib.fileContents "${base}/mozilla/firefox/user.js";
    ".mozilla/firefox/7ty7knlr.default/browser-extension-data/michal.simonfy@gmail.com/storage.js".content = lib.fileContents "${base}/mozilla/firefox/sites.js";

    # Nano
    ".nanorc".content = ''
      set linenumbers
      set rebindkeypad # https://savannah.gnu.org/bugs/?54642
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
        ((modm .|. shiftMask, xK_l), spawn "i3lock-fancy"),
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
           | (key, sc) <- zip [xK_q, xK_w] [0, 1]
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
      export HIST_STAMPS="dd.mm.yyyy"
      export plugins=(docker emacs git gitignore jira kubectl minikube postgres sbt scala ssh-agent)

      # Configure ssh-agent
      zstyle :omz:plugins:ssh-agent agent-forwarding on
      zstyle :omz:plugins:ssh-agent lifetime 4h

      # Point jira command to instance
      export JIRA_URL="jira.esentire.com"
      export JIRA_NAME="tsteinbach"
      export JIRA_PREFIX="ATAD"
      export JIRA_DEFAULT_ACTION="assigned"

      # ENV
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
      alias bluesteel_notifications="nix-shell -p kafkacat --command 'kafkacat -b kaf001cmb01p.internal:9093 -C -t notifications -K \"|\" -o beginning -q -e' | cut -d '|' -f 3 | jq -R 'fromjson?' -c | jq -s 'sort_by(.start) | .[]'"
      alias cat='bat'
      alias diff='diff --color'
      alias fix_screens='xrandr --output DP2-3 --crtc 1 --auto --pos 0x0 --output DP2-2 --crtc 2 --primary --auto --pos 1920x0 --output eDP1 --auto --pos 3840x0'
      alias fix_touchpad='sudo modprobe -r elan_i2c && sleep 5 && sudo modprobe elan_i2c' # ASUS UX305C
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
      function docker_retag() { docker pull $1 && docker tag $1 $2 && docker push $2; }

      # Docker all the things
      DOCKER_KAFKA_IMAGE="solsson/kafka:1.1"
      DOCKER_ZOOKEEPER_IMAGE="zookeeper:3.5"
      function docker_elastic() { docker kill elasticsearch; docker rm elasticsearch; docker run -d --name elasticsearch -p 9200:9200 -e "http.host=0.0.0.0" -e "transport.host=127.0.0.1" registry.internal/common/elasticsearch:5.6.3-noxpack; }
      function docker_mongo() { docker kill mongo; docker rm mongo; docker run -d --name mongo registry.internal/common/mongo mongod --nojournal --smallfiles; }
      function docker_postgres { docker kill postgres; docker rm postgres; docker run -d -p 5432:5432 -e POSTGRES_PASSWORD=postgres --name postgres postgres:9-alpine; }
      function docker_rabbit() { docker kill rabbit; docker rm rabbit; docker run -d -e RABBITMQ_NODENAME=rabbitmq --name rabbit registry.internal/common/rabbitmq; }
      function docker_redis() { docker kill redis; docker rm redis; docker run -d --name redis -p 6379:6379 registry.internal/common/redis:3.0.7; }
      function docker_zk { docker kill zookeeper; docker rm zookeeper; docker run -d -p 2181:2181 --name zookeeper "''${DOCKER_ZOOKEEPER_IMAGE}"; }
      function docker_kafka() { docker kill kafka; docker_zk; docker run -h $(hostname) --rm -d -p 9092:9092 --name kafka --link zookeeper:zookeeper --entrypoint ./bin/kafka-server-start.sh "''${DOCKER_KAFKA_IMAGE}" ./config/server.properties --override zookeeper.connect=zookeeper:2181; }

      # Kafka
      function kafka_consume() { docker run --rm -it --link kafka:kafka --link zookeeper:zookeeper --entrypoint ./bin/kafka-console-consumer.sh "''${DOCKER_KAFKA_IMAGE}" --bootstrap-server kafka:9092 --topic $@;}
      function kafka_consume_key() { docker run --rm -it --link kafka:kafka --link zookeeper:zookeeper --entrypoint ./bin/kafka-console-consumer.sh "''${DOCKER_KAFKA_IMAGE}" --bootstrap-server kafka:9092 --topic $@ --property "print.key=true";}
      function kafka_produce() { docker run --rm -it --link kafka:kafka --link zookeeper:zookeeper --entrypoint ./bin/kafka-console-producer.sh "''${DOCKER_KAFKA_IMAGE}" --broker-list kafka:9092 --topic "$1";}
      function kafka_produce_key() { docker run --rm -it --link kafka:kafka --link zookeeper:zookeeper --entrypoint ./bin/kafka-console-producer.sh "''${DOCKER_KAFKA_IMAGE}" --broker-list kafka:9092 --topic $1 --property "parse.key=true" --property "key.separator=:"; }
      function kafka_topic() { docker run --entrypoint ./bin/kafka-topics.sh --link zookeeper:zookeeper "''${DOCKER_KAFKA_IMAGE}" --zookeeper zookeeper:2181 --create --topic "$1" --if-not-exists --partitions 1 --replication-factor 1; }

      function kafka_prod_topic() { docker run --entrypoint ./bin/kafka-topics.sh --add-host=kaf001cmb01p.internal:10.1.110.136 "''${DOCKER_KAFKA_IMAGE}" --zookeeper kaf001cmb01p.internal:2182 --create --topic "$1" --if-not-exists --partitions 16 --replication-factor 3; }
      function kafka_prod_consume() {  docker run --rm -it --add-host=kaf001cmb01p.internal:10.1.110.136 --entrypoint ./bin/kafka-console-consumer.sh "''${DOCKER_KAFKA_IMAGE}" --bootstrap-server kaf001cmb01p.internal:9093 --topic $@ }

      # Nix review PRs
      function noxpr() { nix-shell -p nox --run "nox-review pr $1"; }

      # Tools
      function sbt() { args="$@"; nix-shell -p openjdk8 -p sbt-extras --command "sbt -J-Xms1G -J-Xmx8G ''${args}"; }
      function bigsbt() { args="$@"; nix-shell -p openjdk8 -p sbt-extras -p nodejs -p jekyll --command "sbt -J-Xms1G -J-Xmx8G ''${args}"; }
      function amm() { nix-shell -p ammonite --command "amm"; }
      function travis() { args="$@"; nix-shell -p travis --command "travis ''${args}"; }
    '';
  };
}
