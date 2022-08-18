import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Paste
import XMonad.Hooks.ServerMode

import qualified XMonad.StackSet as W

import Data.List (isPrefixOf)

smartNextWindow :: X ()
smartNextWindow = do
    mtitle <- logTitle
    case mtitle of
        Nothing -> return ()
        Just title ->
            if "emacs" `isPrefixOf` title
            then sendKey mod1Mask xK_o
            else windows W.focusDown

smartPrevWindow :: X ()
smartPrevWindow = do
    mtitle <- logTitle
    case mtitle of
        Nothing -> return ()
        Just title ->
            if "emacs" `isPrefixOf` title
            then sendKey (mod1Mask .|. shiftMask) xK_o
            else windows W.focusUp

translateIfNotEmacs ::
       (KeyMask, KeySym) -> [(KeyMask, KeySym)] -> ((KeyMask, KeySym), X ())
translateIfNotEmacs (maskF, keyF) keySeq =
    ( (maskF, keyF)
    , do mtitle <- logTitle
         case mtitle of
             Nothing -> return ()
             Just title ->
                 if "emacs" `isPrefixOf` title
                 then sendKey maskF keyF
                 else sequence_ $ uncurry sendKey <$> keySeq)

copy :: ((KeyMask, KeySym), X ())
copy = translateIfNotEmacs (mod1Mask, xK_w) [(controlMask, xK_c)]

paste :: ((KeyMask, KeySym), X ())
paste = translateIfNotEmacs (controlMask, xK_y) [(controlMask, xK_v)]

lineDown :: ((KeyMask, KeySym), X ())
lineDown = translateIfNotEmacs (controlMask, xK_n) [(noModMask, xK_Down)]

lineUp :: ((KeyMask, KeySym), X ())
lineUp = translateIfNotEmacs (controlMask, xK_p) [(noModMask, xK_Up)]

search :: ((KeyMask, KeySym), X ())
search = translateIfNotEmacs (controlMask, xK_s) [(controlMask, xK_f)]

paraDown :: ((KeyMask, KeySym), X ())
paraDown = translateIfNotEmacs (mod1Mask, xK_n) [(noModMask, xK_Next)]

paraUp :: ((KeyMask, KeySym), X ())
paraUp = translateIfNotEmacs (mod1Mask, xK_p) [(noModMask, xK_Prior)]

killLine :: ((KeyMask, KeySym), X ())
killLine =
    translateIfNotEmacs
        (controlMask, xK_k)
        [(shiftMask, xK_End), (controlMask, xK_x)]

movePointBegin :: ((KeyMask, KeySym), X ())
movePointBegin = translateIfNotEmacs (controlMask, xK_a) [(noModMask, xK_Home)]

charBack :: ((KeyMask, KeySym), X ())
charBack = translateIfNotEmacs (controlMask, xK_b) [(noModMask, xK_Left)]

charForward :: ((KeyMask, KeySym), X ())
charForward = translateIfNotEmacs (controlMask, xK_f) [(noModMask, xK_Right)]

wordBack :: ((KeyMask, KeySym), X ())
wordBack = translateIfNotEmacs (mod1Mask, xK_b) [(controlMask, xK_Left)]

wordForward :: ((KeyMask, KeySym), X ())
wordForward = translateIfNotEmacs (mod1Mask, xK_f) [(controlMask, xK_Right)]

-- XXX There is something wrong with this
esc :: ((KeyMask, KeySym), X ())
esc = translateIfNotEmacs (controlMask, xK_g) [(noModMask, xK_Escape)]

main :: IO ()
main = xmonad conf

    where

    conf =
        flip
            additionalKeys
            [ ((mod1Mask, xK_o), smartNextWindow)
            , ((mod1Mask .|. shiftMask, xK_o), smartPrevWindow)
            , copy
            , paste
            , lineDown
            , lineUp
            , search
            , paraDown
            , paraUp
            -- , esc
            , killLine
            , movePointBegin
            , charBack
            , charForward
            , wordBack
            , wordForward
            ]
            $ def
                  { terminal = "/usr/local/bin/st"
                  , modMask = mod4Mask
                  , handleEventHook = serverModeEventHook
                  }
