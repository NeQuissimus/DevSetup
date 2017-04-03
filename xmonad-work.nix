{ config, lib, pkgs, ... }:
rec {
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    extraPackages = with pkgs.haskellPackages; haskellPackages: [ xmobar ];
  };

  # See https://github.com/NixOS/nixpkgs/issues/20258
  environment.etc."xmonad/xmonad.hs".text = ''
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
    , className =? "Franz" --> doShift "0"
    , className =? "Firefox" --> viewShift "9"
    , className =? "Sublime" --> viewShift "2"
    , appName =? "desktop_window" --> doIgnore
    ]
    where viewShift = doF . liftM2 (.) W.greedyView W.shift
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $ [
  ((modm, xK_Return), spawn $ XMonad.terminal conf),
  ((modm .|. shiftMask, xK_l), spawn "${pkgs.i3lock-fancy}/bin/i3lock-fancy"),
  ((modm, xK_d), spawn "${pkgs.dmenu}/bin/dmenu_run"),
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
     | (key, sc) <- zip [xK_w, xK_q, xK_e] [0, 1, 2]
     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
main = do
  xmproc <- spawnPipe "${pkgs.haskellPackages.xmobar}/bin/xmobar /etc/xmobar/config"
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
    normalBorderColor = "#666666",
    terminal = "${pkgs.rxvt_unicode-with-plugins}/bin/urxvtc",
    workspaces = myWorkspaces
  }
  '';

  environment.etc."xmobar/config".text = ''
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
                    , Run MultiCpu [ "--template" , "Cpu: <total0>% | <total1>% | <total2>% | <total3>%" ] 10
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
}