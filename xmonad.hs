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
import XMonad.Layout.Tabbed

import qualified Data.Map as M
import qualified XMonad.StackSet as W

main :: IO ()
main = do
    dirs <- getDirectories
    xmonad $ ewmhFullscreen $ ewmh $ xmobarProp $ conf

    where

    conf =
        def
            { terminal = "st"
            , modMask = mod4Mask
            , keys = customKeys
            , layoutHook = simpleTabbed
            }

ifElseMoreThanOneWindow :: X () -> X () -> X ()
ifElseMoreThanOneWindow left right = do
    state <- get
    let currentStack = W.stack $ W.workspace $ W.current $ windowset state
    let numWindows = length $ W.integrate' currentStack
    if numWindows > 1
    then left
    else right

customKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
customKeys conf = M.fromList $
    -- launching and killing programs
    [ ((controlMask  .|. shiftMask, xK_Return), spawn "dmenu_run -l 10 -b")
    , ((mod1Mask .|. shiftMask, xK_c        ), kill)

    -- move focus up or down the window stack
    , ( (controlMask .|. shiftMask, xK_n)
      , ifElseMoreThanOneWindow
            (windows W.focusDown)
            (sendKey (controlMask .|. shiftMask) xK_n)
      )
    , ( (controlMask .|. shiftMask, xK_p)
      , ifElseMoreThanOneWindow
            (windows W.focusUp)
            (sendKey (controlMask .|. shiftMask) xK_p)
      )

    -- resizing the master/slave ratio
    , ((mod1Mask,               xK_Left       ), windows W.swapUp)
    , ((mod1Mask,               xK_Right      ), windows W.swapDown)

    -- quit, or restart
    -- , ((mod1Mask .|. shiftMask, xK_q     ), io exitSuccess) -- %! Quit xmonad

    , ((mod1Mask .|. shiftMask, xK_slash ), helpCommand)
    -- repeat the binding for non-American layout keyboards
    , ((mod1Mask              , xK_question), helpCommand)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((mod1Mask, k), windows $ W.greedyView i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]]
    ++
    [((shiftMask .|. mod4Mask, k), windows $ W.shift i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    where

    helpCommand :: X ()
    helpCommand = xmessage "See the xmonad.hs for all the bindings"
