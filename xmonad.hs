import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Paste
import XMonad.Hooks.ServerMode
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.NamedWindows

import qualified Data.Map as M
import qualified XMonad.StackSet as W

main :: IO ()
main = do
    dirs <- getDirectories
    flip launch dirs $ ewmhFullscreen $ ewmh $ xmobarProp $ conf

    where

    conf =
        def
            { terminal = "st"
            , modMask = mod1Mask
            , keys = customKeys
            }

customKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
customKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. shiftMask, xK_0     ), spawn "rofi -show run")
    , ((controlMask .|. shiftMask, xK_Return ), spawn "rofi -show window")
    , ((modMask .|. shiftMask, xK_c        ), kill)

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp  )

    -- modifying the window order
    , ((modMask  .|. shiftMask, xK_Return), windows W.swapMaster)

    -- resizing the master/slave ratio
    , ((modMask,               xK_Right     ), sendMessage Shrink)
    , ((modMask,               xK_Left      ), sendMessage Expand)

    -- quit, or restart
    -- , ((modMask .|. shiftMask, xK_q     ), io exitSuccess) -- %! Quit xmonad

    , ((modMask .|. shiftMask, xK_slash ), helpCommand)
    -- repeat the binding for non-American layout keyboards
    , ((modMask              , xK_question), helpCommand)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    where

    helpCommand :: X ()
    helpCommand = xmessage "See the xmonad.hs for all the bindings"
