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

      ;;; Fira code
      (set-default-font "Fira Code")

      ;; This works when using emacs --daemon + emacsclient
      (add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
      ;; This works when using emacs without server/client
      (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
      ;; I haven't found one statement that makes both of the above situations work, so I use both for now

      (defconst fira-code-font-lock-keywords-alist
        (mapcar (lambda (regex-char-pair)
                  `(,(car regex-char-pair)
                    (0 (prog1 ()
                         (compose-region (match-beginning 1)
                                         (match-end 1)
                                         ;; The first argument to concat is a string containing a literal tab
                                         ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
                '(("\\(www\\)"                   #Xe100)
                  ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
                  ("\\(\\*\\*\\*\\)"             #Xe102)
                  ("\\(\\*\\*/\\)"               #Xe103)
                  ("\\(\\*>\\)"                  #Xe104)
                  ("[^*]\\(\\*/\\)"              #Xe105)
                  ("\\(\\\\\\\\\\)"              #Xe106)
                  ("\\(\\\\\\\\\\\\\\)"          #Xe107)
                  ("\\({-\\)"                    #Xe108)
                  ;;("\\(\\[\\]\\)"                #Xe109)
                  ("\\(::\\)"                    #Xe10a)
                  ("\\(:::\\)"                   #Xe10b)
                  ("[^=]\\(:=\\)"                #Xe10c)
                  ("\\(!!\\)"                    #Xe10d)
                  ("\\(!=\\)"                    #Xe10e)
                  ("\\(!==\\)"                   #Xe10f)
                  ("\\(-}\\)"                    #Xe110)
                  ("\\(--\\)"                    #Xe111)
                  ("\\(---\\)"                   #Xe112)
                  ("\\(-->\\)"                   #Xe113)
                  ("[^-]\\(->\\)"                #Xe114)
                  ("\\(->>\\)"                   #Xe115)
                  ("\\(-<\\)"                    #Xe116)
                  ("\\(-<<\\)"                   #Xe117)
                  ("\\(-~\\)"                    #Xe118)
                  ("\\(#{\\)"                    #Xe119)
                  ("\\(#\\[\\)"                  #Xe11a)
                  ("\\(##\\)"                    #Xe11b)
                  ("\\(###\\)"                   #Xe11c)
                  ("\\(####\\)"                  #Xe11d)
                  ("\\(#(\\)"                    #Xe11e)
                  ("\\(#\\?\\)"                  #Xe11f)
                  ("\\(#_\\)"                    #Xe120)
                  ("\\(#_(\\)"                   #Xe121)
                  ("\\(\\.-\\)"                  #Xe122)
                  ("\\(\\.=\\)"                  #Xe123)
                  ("\\(\\.\\.\\)"                #Xe124)
                  ("\\(\\.\\.<\\)"               #Xe125)
                  ("\\(\\.\\.\\.\\)"             #Xe126)
                  ("\\(\\?=\\)"                  #Xe127)
                  ("\\(\\?\\?\\)"                #Xe128)
                  ("\\(;;\\)"                    #Xe129)
                  ("\\(/\\*\\)"                  #Xe12a)
                  ("\\(/\\*\\*\\)"               #Xe12b)
                  ("\\(/=\\)"                    #Xe12c)
                  ("\\(/==\\)"                   #Xe12d)
                  ("\\(/>\\)"                    #Xe12e)
                  ("\\(//\\)"                    #Xe12f)
                  ("\\(///\\)"                   #Xe130)
                  ("\\(&&\\)"                    #Xe131)
                  ("\\(||\\)"                    #Xe132)
                  ("\\(||=\\)"                   #Xe133)
                  ("[^|]\\(|=\\)"                #Xe134)
                  ("\\(|>\\)"                    #Xe135)
                  ("\\(\\^=\\)"                  #Xe136)
                  ("\\(\\$>\\)"                  #Xe137)
                  ("\\(\\+\\+\\)"                #Xe138)
                  ("\\(\\+\\+\\+\\)"             #Xe139)
                  ("\\(\\+>\\)"                  #Xe13a)
                  ("\\(=:=\\)"                   #Xe13b)
                  ("[^!/]\\(==\\)[^>]"           #Xe13c)
                  ("\\(===\\)"                   #Xe13d)
                  ("\\(==>\\)"                   #Xe13e)
                  ("[^=]\\(=>\\)"                #Xe13f)
                  ("\\(=>>\\)"                   #Xe140)
                  ("\\(<=\\)"                    #Xe141)
                  ("\\(=<<\\)"                   #Xe142)
                  ("\\(=/=\\)"                   #Xe143)
                  ("\\(>-\\)"                    #Xe144)
                  ("\\(>=\\)"                    #Xe145)
                  ("\\(>=>\\)"                   #Xe146)
                  ("[^-=]\\(>>\\)"               #Xe147)
                  ("\\(>>-\\)"                   #Xe148)
                  ("\\(>>=\\)"                   #Xe149)
                  ("\\(>>>\\)"                   #Xe14a)
                  ("\\(<\\*\\)"                  #Xe14b)
                  ("\\(<\\*>\\)"                 #Xe14c)
                  ("\\(<|\\)"                    #Xe14d)
                  ("\\(<|>\\)"                   #Xe14e)
                  ("\\(<\\$\\)"                  #Xe14f)
                  ("\\(<\\$>\\)"                 #Xe150)
                  ("\\(<!--\\)"                  #Xe151)
                  ("\\(<-\\)"                    #Xe152)
                  ("\\(<--\\)"                   #Xe153)
                  ("\\(<->\\)"                   #Xe154)
                  ("\\(<\\+\\)"                  #Xe155)
                  ("\\(<\\+>\\)"                 #Xe156)
                  ("\\(<=\\)"                    #Xe157)
                  ("\\(<==\\)"                   #Xe158)
                  ("\\(<=>\\)"                   #Xe159)
                  ("\\(<=<\\)"                   #Xe15a)
                  ("\\(<>\\)"                    #Xe15b)
                  ("[^-=]\\(<<\\)"               #Xe15c)
                  ("\\(<<-\\)"                   #Xe15d)
                  ("\\(<<=\\)"                   #Xe15e)
                  ("\\(<<<\\)"                   #Xe15f)
                  ("\\(<~\\)"                    #Xe160)
                  ("\\(<~~\\)"                   #Xe161)
                  ("\\(</\\)"                    #Xe162)
                  ("\\(</>\\)"                   #Xe163)
                  ("\\(~@\\)"                    #Xe164)
                  ("\\(~-\\)"                    #Xe165)
                  ("\\(~=\\)"                    #Xe166)
                  ("\\(~>\\)"                    #Xe167)
                  ("[^<]\\(~~\\)"                #Xe168)
                  ("\\(~~>\\)"                   #Xe169)
                  ("\\(%%\\)"                    #Xe16a)
                 ;; ("\\(x\\)"                   #Xe16b) This ended up being hard to do properly so i'm leaving it out.
                  ("[^:=]\\(:\\)[^:=]"           #Xe16c)
                  ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
                  ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

      (defun add-fira-code-symbol-keywords ()
        (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

      (add-hook 'prog-mode-hook
                #'add-fira-code-symbol-keywords)

      ;; end FiraCode

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

#    ".nanorc".content = ''
#      include ~/.nano/syntax/ALL.nanorc
#      set const
#    '';

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
        ((modm .|. shiftMask, xK_l), spawn "$i3lock-fancy"),
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
      ZSH_THEME="nequissimus"
      ZSH_CUSTOM="/home/${user}/dev/nequi-zsh"

      CASE_SENSITIVE="false"
      HIST_STAMPS="dd.mm.yyyy"
      plugins=(docker git gitignore)

      export TERMINAL="xterm"
      export JAVA_HOME="''${$(readlink -e $(type -p java))%*/bin/java}"

      export GOOGLE_APPLICATION_CREDENTIALS="''${HOME}/Documents/service_account.json"
      export CLOUDSDK_CONTAINER_USE_APPLICATION_DEFAULT_CREDENTIALS=true

      [[ -e "''${ZSH}/oh-my-zsh.sh" ]] && source "''${ZSH}/oh-my-zsh.sh"

      bindkey "^[Od" backward-word
      bindkey "^[Oc" forward-word

      alias diff='diff --color'
      alias docker_clean_dangling='docker images -qf dangling=true | xargs -r docker rmi'
      alias docker_clean_images='docker kill $(docker ps -q); docker rm $(docker ps -a -q); docker rmi -f $(docker images -q)'
      alias docker_clean='docker kill $(docker ps -q); docker rm $(docker ps -a -q)'
      alias fix_screens='xrandr --output DP2-3 --crtc 1 --auto --pos 0x0 --output DP2-2 --crtc 2 --primary --auto --pos 1920x0 --output eDP1 --auto --pos 3840x0'
      alias fix_touchpad='sudo modprobe -r elan_i2c && sleep 5 && sudo modprobe elan_i2c' # ASUS UX305C
      alias ls='exa'
      alias mvn='mvn -q'
      alias nano='nano -E -w -c'
      alias nix-repair-store='nix-store --verify --check-contents --repair'
      alias rsync='rsync -azvvP'
      alias sbt='sbt -J-Xms1G -J-Xmx8G'
      alias scrot='scrot -s'
      alias volume_down='amixer -q sset Master 5%-; volume'
      alias volume_up='amixer -q sset Master 5%+; volume'
      alias volume='awk -F"[][]" "/dB/ { print $2 }" <(amixer sget Master)'
      alias vpn='sudo openconnect --user=tsteinbach --authgroup=RSA cmbvpn.esentire.com --mtu 1492'

      function docker_elastic() { docker kill elasticsearch; docker rm elasticsearch; docker run -d --name elasticsearch -p 9200:9200 -e "http.host=0.0.0.0" -e "transport.host=127.0.0.1" registry.internal/common/elasticsearch:5.6.3-noxpack }
      function docker_inspect() { (skopeo inspect docker://"$1" || docker inspect "$1") | jq }
      function docker_kazoo() { docker run --rm --name kazoo --link zookeeper:zookeeper -v $(pwd)/$1:/config.json "registry.internal/soa/kazoo:0.0.1" }
      function docker_mongo() { docker kill mongo; docker rm mongo; docker run -d --name mongo registry.internal/common/mongo mongod --nojournal --smallfiles }
      function docker_postgres { docker kill postgres; docker rm postgres; docker run -d -p 5432:5432 --name postgres postgres:9-alpine }
      function docker_rabbit() { docker kill rabbit; docker rm rabbit; docker run -d -e RABBITMQ_NODENAME=rabbitmq --name rabbit registry.internal/common/rabbitmq }
      function docker_redis() { docker kill redis; docker rm redis; docker run -d --name redis -p 6379:6379 registry.internal/common/redis:3.0.7 }
      function docker_retag() { docker pull $1 && docker tag $1 $2 && docker push $2 }
      function docker_zk { docker kill zookeeper; docker rm zookeeper; docker run -d -p 2181:2181 --name zookeeper zookeeper:3.4.10 }

      function docker_kafka() {
        docker kill kafka
        docker_zk
        docker run -h $(hostname) --rm -d -p 9092:9092 --name kafka --link zookeeper:zookeeper --entrypoint ./bin/kafka-server-start.sh solsson/kafka:1.0.0 ./config/server.properties --override zookeeper.connect=zookeeper:2181
      }
      function kafka_consume() {
        docker run --rm -it --link kafka:kafka --link zookeeper:zookeeper --entrypoint ./bin/kafka-console-consumer.sh solsson/kafka:1.0.0 --bootstrap-server kafka:9092 --topic $@
      }
      function kafka_produce() {
        docker run --rm -it --link kafka:kafka --link zookeeper:zookeeper --entrypoint ./bin/kafka-console-producer.sh solsson/kafka:1.0.0 --broker-list kafka:9092 --topic $1
      }
      function kafka_topic() {
        docker run --entrypoint ./bin/kafka-topics.sh --link zookeeper:zookeeper solsson/kafka:1.0.0 --zookeeper zookeeper:2181 --create --topic "$1" --if-not-exists --partitions 1 --replication-factor 1
      }

      function noxpr() { nix-shell -p nox --run "nox-review pr $1" }
      function vpn_route() { sudo route add -net 10.203.0.0 netmask 255.255.0.0 dev tun0; sudo route add -net 10.1.0.0 netmask 255.255.0.0 dev tun0 }

      unsetopt correct_all

      type -p kubectl >> /dev/null && source <(kubectl completion zsh)
      type -p minikube >> /dev/null && source <(minikube completion zsh)
    '';
  };
}
