{ pkgs, config, ... }: {
  home.file = {
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
  };

  xsession.windowManager.xmonad = {
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
