-- Imports:
import XMonad
import Data.Monoid
import System.Exit

import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.MultiColumns

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

import XMonad.Util.Run
import XMonad.Util.SpawnOnce (spawnOnce)

import qualified Xmonad.StackSet as W
import qualified Data.Map as M

-- Prefered Terminal Program:
myTerminal = "alacritty"

-- Sloppy Selection, Whether focus follow the mouse pointer
myFocusFollowsMouse :: Bool = False
-- However, if you click on the window, that passes focus
myClickJustFocuses :: Bool = True

-- Width of the Window border (pixels):
myBorderWidth = 5

-- Choosing The 'Super/Windows' key as the Mod Key
myModMask = mod4Mask

-- Naming and Creating Workspaces
myWorkspaces = [
    "Alpha",
    "Beta",
    "Gamma",
    "Delta",
    "Epsilon",
    "Zeta",
    "Eta",
    "Theta",
    "Iota",
    "Kappa",
]

-- Border colours for unfocused and focused windows
myNormalBorderColor = "#3c3836"
myFocusedBorderColor = "#fbf1c7"

-- Key Bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
        ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf), -- Launch Terminal
        ((modm, xK_Return), spawn $ XMonad.terminal conf), -- Launch Terminal other methoo
        ((modm, xK_d), spawn "dmenu_run"), -- Launch dmenu

        ((modm .|. shiftMask, xK_q), kill), -- Kill selected window

        ((modm, xK_space), sendMessage NextLayout), -- Rotate to next layout
        ((modm, xK_n), refresh), -- Resize selected window to correct size

        ((modm, xK_j), windows W.focusDown), -- Move focus to Next Window
        ((modm, xK_k), windows W.focusUp), -- Move focus to Previous Window
        ((modm, xK_Tab), windows W.focusMaster), -- Move focus to Master Window

        ((modm .|. shiftMask, xK_j), windows W.swapDown), -- Swap focused window with next window
        ((modm .|. shiftMask, xK_k), windows W.swapUp), -- Swap focus window with previous window
        ((modm .|. shiftMask, xK_Tab), windows W.swapMaster), -- Swap focus with master window

        ((modm, xK_h), sendMessage Shrink), -- Shrink Master Area
        ((modm, xK_l), sendMessage Expand), -- Expand Master Area

        ((modm, xK_t), withFocused $ windows . W.sink), -- Toggle tiling on window

        ((modm, xK_comma), sendMessage (IncMasterN 1)), -- Increase number of windows in Master Area
        ((modm, xK_period), sendMessage (IncMasterN (-1))), -- Decrease number of windows in Master Area

        ((modm .|. shiftMask, xK_e), io (exitWith ExitSuccess)),-- Quit XMonad
        ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart") -- Restart Xmonad
    ]
    ++

    -- Switch to workspace N (mod N)
    -- Move window to workspace N (M-Mod N)

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- Support for multiple monitors (not that I have more than 1 anyway)
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xk_w, xK_r, xK_e] [0..]
        , (f, m) <- [(W.view), 0], (W.shift, shiftMask)]

-- Mouse bindings
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [
        -- Mod-button1, move and float the window
        ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                         >> windows W.shiftMaster)),

        -- Mod-button2, Raise the window to the top of the stack
        ((modm, button2), (\w -> focus w >> windows W.shiftMaster)),

        -- Mod-button3, set the window to floating mode and resize by dragging
        ((modm, button3), (\w -> focus w >> mouseResizeWindow W
                                         >> windows W.shiftMaster))
    ]

-- Layouts:
myLayout = avoidStruts (tilled ||| Mirror tiled ||| Full)
  where
      tiled = spacing 10 $ Tall nmaster delta ration
      nmaster = 1
      ratio = 1/2
      delta = 3/100

-- Window Rules:
myManageHook = composeAll
[
    className =? "Mplayer" --> doFloat,
    resource =? "desktop_window" --> doIgnore,
    resource =? "kdesktop" --> doIgnore
]

myEventHook = mconcat [ fullscreenEventHook ]

myLogHook = return ()

-- Startup
myStartupHook = do
    spawn "/home/maxm/.config/polybar/launch.sh"
    swapn "picom --experimental-backends"
    spawnOnce "nitrogen --restore"
    spawnOnce "setxkbmap gb"

main = xmonad $ docks default

defaults = def {
    -- Simple stuff
    terminal = myTerminal,
    focusFollowsMouse = myFocusFollowsMouse,
    clickJustFocuses = myClickJustFocuses,
    borderWidth = myBorderWidth,
    modMask = myModMask,
    workspaces = myWorkspaces,
    normalBorderColor = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- Keybindings
    keys = myKeys,
    mouseBindings = myMouseBindings,

    -- Hooks and Layouts
    layoutHook = myLayout,
    manageHook = myManageHook,
    handleEventHook = myEventHook,
    logHook = myLogHook,
    startupHook = myStartupHook
}