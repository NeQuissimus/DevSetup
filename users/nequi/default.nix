with import <nixpkgs> {};
with import <nixhome> { inherit stdenv; inherit pkgs; };
with lib;
let
  user = "nequi";
in mkHome {
  inherit user;

  files = {
    ".config/rofi/config".content = "rofi.theme: ${pkgs.rofi-unwrapped}/share/rofi/themes/sidebar.rasi";

    ".emacs.d/init.el".content = ''
      (scroll-bar-mode -1)
      (package-initialize)

      (load-theme 'zerodark t)
      (zerodark-setup-modeline-format)

      (setq
       inhibit-startup-screen t
       create-lockfiles nil
       make-backup-files nil
       column-number-mode t
       scroll-error-top-bottom t
       show-paren-delay 0.5
       use-package-always-ensure t
       show-trailing-whitespace t
       sentence-end-double-space nil)

      (set-language-environment "UTF-8")
      (set-default-coding-systems 'utf-8)

      (setq whitespace-style '(face trailing tabs))
      (custom-set-faces
       '(whitespace-tab ((t (:background "red")))))
      (global-whitespace-mode)

      ;; Load files from disk when changed
      (global-auto-revert-mode t)

      ;; Remove white-spaces when saving
      (add-hook 'before-save-hook 'delete-trailing-whitespace)

      ;; buffer local variables
      (setq-default
       indent-tabs-mode nil
       tab-width 2
       c-basic-offset 2
       buffer-file-coding-system 'utf-8-unix)

      ;; modes
      (electric-indent-mode 0)

      ;; global keybindings
      (global-unset-key (kbd "C-z"))
      (global-set-key (kbd "C-x f") 'projectile-find-file)
      (define-key global-map (kbd "RET") 'newline-and-indent)
      (global-set-key (kbd "C-x g") 'magit-status)

      ;; Allocate more memory
      (setq gc-cons-threshold 20000000)

      (require 'smartparens-config)
      (smartparens-global-mode)

      (projectile-global-mode)

      ;; Gitter + Irc
      (use-package erc
        :commands erc erc-tls
        :init
        (setq
         erc-prompt-for-password t ;; prefer ~/.authinfo for passwords
         erc-hide-list '("JOIN" "PART" "QUIT")
         erc-autojoin-channels-alist
         '(("irc.gitter.im" "#scalaz/scalaz"))))

      (defun gitter()
        "Connect to Gitter."
        (interactive)
        (erc-tls :server "irc.gitter.im" :port 6697))


      (set-default-font "Hasklig")

      (defun my-correct-symbol-bounds (pretty-alist)
        (mapcar (lambda (el)
              (setcdr el (string ?\t (cdr el)))
                el)
            pretty-alist))

      (defun my-ligature-list (ligatures codepoint-start)
        (let ((codepoints (-iterate '1+ codepoint-start (length ligatures))))
          (-zip-pair ligatures codepoints)))

      (setq my-hasklig-ligatures
        (let* ((ligs '("&&" "***" "*>" "\\\\" "||" "|>" "::"
                   "==" "===" "==>" "=>" "=<<" "!!" ">>"
                   ">>=" ">>>" ">>-" ">-" "->" "-<" "-<<"
                   "<*" "<*>" "<|" "<|>" "<$>" "<>" "<-"
                   "<<" "<<<" "<+>" ".." "..." "++" "+++"
                   "/=" ":::" ">=>" "->>" "<=>" "<=<" "<->")))
      (my-correct-symbol-bounds (my-ligature-list ligs #Xe100))))

      ;; nice glyphs for haskell with hasklig
      (defun my-set-hasklig-ligatures ()
        (setq prettify-symbols-alist
          (append my-hasklig-ligatures prettify-symbols-alist))
        (prettify-symbols-mode))

      (add-hook 'prog-mode-hook 'my-set-hasklig-ligatures)

      (ac-config-default)
    '';

    ".gitconfig".content = ''
      [alias]
              bclean = "!f() { git fetch --prune && git branch --merged ''${1-master} | grep -v " ''${1-master}$" | xargs -r git branch -d && git branch -r | awk '{print $1}' | egrep -v -f /dev/fd/0 <(git branch -vv | grep origin) | awk '{print $1}' | xargs -r git branch -d; }; f"
              clear = clean -xfd
              findcommit = !"git rev-list --all | xargs git grep '$1'"
              hard = reset --hard origin/master
              lastcommit = for-each-ref --sort=committerdate refs --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(contents:subject) - %ae (%(color:green)%(committerdate:relative)%(color:reset))'
              lg = log --all --decorate --color --graph --pretty=format:'%Cred%h%Creset %Cgreen(%cr)%Creset - %s %C(bold blue)<%an>[%G?]%Creset%C(auto)%d%Creset' --abbrev-commit
              ll = log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate --numstat
              pr = "!f() { git fetch origin pull/''${1}/head:pr-''${1}; git checkout pr-''${1}; }; f"
              prup = "!f() { git fetch upstream pull/''${1}/head:pr-''${1}; git checkout pr-''${1}; }; f"
              rename = "!f() { git push -u origin origin/"''${1}":refs/heads/"''${2}"; git push origin :"''${1}"; }; f"
              stable = "!f() { git checkout release-18.03  && git pull && git cherry-pick -x ''${1} && git push && git checkout master; }; f"
              t = tag -s -a
              undo = reset HEAD~1 --mixed
              up = !git pull --rebase --prune $@ && git submodule update --init --recursive
      [apply]
              whitespace = fix
      [branch]
              autosetuprebase = always
      [commit]
              gpgsign = true
      [core]
              excludesfile = ~/.gitignore
              pager = less
              whitespace=fix,-indent-with-non-tab,trailing-space,cr-at-eol
      [diff]
              compactionHeuristic = true
      [interactive]
              diffFilter = diff-highlight
      [pull]
              rebase = true
      [push]
              default = upstream
              followTags = true
      [tag]
              forceSignAnnotated = true
      [user]
              email = tim@nequissimus.com
              name = Tim Steinbach
              signingkey = 0588CEBD610D7123
    '';

    ".Xdefaults".content = ''
      *background: #222222
      *foreground: #babdb6
      *color0: #000000
      *color8: #555753
      *color1: #ff6565
      *color9: #ff8d8d
      *color2: #93d44f
      *color10: #c8e7a8
      *color3: #eab93d
      *color11: #ffc123
      *color4: #604a87
      *color12: #3465a4
      *color5: #ce5c00
      *color13: #f57900
      *color6: #89b6e2
      *color14: #46a4ff
      *color7: #cccccc
      *color15: #ffffff

      Xft.dpi: 96
      Xft.antialias: true
      Xft.rgba: rgb
      Xft.hinting: true
      Xft.hintstyle: hintslight

      URxvt*font: xft:DejaVu Sans Mono:size=12,xft:Monospace:size=12
      URxvt*geometry: 112x22
      URxvt*urlLauncher: firefox
      URxvt*scrollBar: false
      URxvt*scrollBar_right: true
      URxvt*scrollColor: #000000
      URxvt*scrollTtyKeypress: true
      URxvt*scrollTtyOutput: false
      URxvt*scrollWithBuffer: true
      URxvt.saveLines: 250000
      URxvt*iso14755: False
    '';

    ".Xresources".content = ''
      Emacs*toolBar: 0
      Emacs*menuBar: 0
    '';

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
      myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" ]
      myManageHook = composeAll [
          isFullscreen --> (doF W.focusDown <+> doFullFloat)
          , isDialog --> doFloat
          , className =? "Chromium-browser" --> viewShift "8"
          , className =? "Firefox" --> viewShift "9"
          , className =? "Emacs" --> viewShift "2"
          , className =? "Atom" --> viewShift "2"
          , className =? "Code" --> viewShift "2"
          , className =? "HipChat" --> viewShift "0"
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
      main = do
        xmproc <- spawnPipe "xmobar /home/${user}/.xmobar/config"
        xmonad $ def {
          focusedBorderColor = "#F0F0F0",
          handleEventHook = docksEventHook <+> handleEventHook def,
          keys = myKeys,
          layoutHook = avoidStruts $ layoutHook def,
          logHook = dynamicLogWithPP xmobarPP
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
         Config { font = "xft:Font Awesome:size=9, xft:DejaVu Sans Mono:size=8:antialias=true"
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
            , template = "%StdinReader% }{  %multicpu% |  %memory% MiB |  %dynnetwork% |  %date% |  %battery% |  %uname%"
            }
    '';

    ".zshrc".content = ''
      #!/usr/env/bin zsh

      # oh-my-zsh
      export ZSH_THEME="nequissimus"
      export ZSH_CUSTOM="/home/${user}/dev/nequi-zsh"
      export CASE_SENSITIVE="false"
      export HIST_STAMPS="dd.mm.yyyy"
      export plugins=(docker git gitignore)

      # ENV
      export TERMINAL="xterm"
      export TERM="linux"
      export JAVA_HOME="''${$(readlink -e $(type -p java))%*/bin/java}"

      export GOOGLE_APPLICATION_CREDENTIALS="''${HOME}/Documents/service_account.json"
      export CLOUDSDK_CONTAINER_USE_APPLICATION_DEFAULT_CREDENTIALS=true

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
      alias diff='diff --color'
      alias fix_screens='xrandr --output DP2-3 --crtc 1 --auto --pos 0x0 --output DP2-2 --crtc 2 --primary --auto --pos 1920x0 --output eDP1 --auto --pos 3840x0'
      alias fix_touchpad='sudo modprobe -r elan_i2c && sleep 5 && sudo modprobe elan_i2c' # ASUS UX305C
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
      function docker_postgres { docker kill postgres; docker rm postgres; docker run -d -p 5432:5432 --name postgres postgres:9-alpine; }
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

      function kafka_prod_topic() { docker run --add-host=kaf001cmb01p.internal:10.1.110.136 --entrypoint ./bin/kafka-topics.sh "''${DOCKER_KAFKA_IMAGE}" --zookeeper kaf001cmb01p.internal:2182 --describe --topic "$1" }
      function kafka_prod_consume() {  docker run --rm -it --add-host=kaf001cmb01p.internal:10.1.110.136 --entrypoint ./bin/kafka-console-consumer.sh "''${DOCKER_KAFKA_IMAGE}" --bootstrap-server kaf001cmb01p.internal:9093 --topic $@ }

      # Nix review PRs
      function noxpr() { nix-shell -p nox --run "nox-review pr $1"; }

      # Tools
      function sbt() { args="$@"; nix-shell -p sbt-extras -p nodejs -p jekyll --command "sbt -J-Xms1G -J-Xmx8G ''${args}"; }
      function amm() { nix-shell -p ammonite --command "amm"; }
      function travis() { args="$@"; nix-shell -p travis --command "travis ''${args}"; }
      function kubectl() { args="$@"; nix-shell -p kubectl --command "kubectl ''${args}"; }

      # Include shell completions
      type -p kubectl >> /dev/null && source <(kubectl completion zsh)
      type -p minikube >> /dev/null && source <(minikube completion zsh)
    '';
  };
}
