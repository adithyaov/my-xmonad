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
            ]
            $ def
                  { terminal = "/usr/local/bin/st"
                  , modMask = mod4Mask
                  , handleEventHook = serverModeEventHook
                  }
