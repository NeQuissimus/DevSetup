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
    , className =? "HipChat" --> viewShift "0"
    , className =? "Sublime" --> viewShift "2"
    , className =? "Atom" --> viewShift "2"
    , appName =? "desktop_window" --> doIgnore
    ]
    where viewShift = doF . liftM2 (.) W.greedyView W.shift

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $ [
  ((modm, xK_Return), spawn $ XMonad.terminal conf),
  ((modm .|. shiftMask, xK_l), spawn "/home/tsteinbach/dev/i3lock-fancy-multimonitor/lock"),
  ((modm, xK_d), spawn "dmenu_run"),
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
     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/tsteinbach/.xmobar/config"
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
    terminal = "xterm",
    workspaces = myWorkspaces
  }
