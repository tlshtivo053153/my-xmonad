module MyConfig.Xmobar where

import System.IO ( Handle, hPutStrLn )

import qualified Data.Char as Char
import qualified Data.List as L

import XMonad
import XMonad.Hooks.DynamicLog

myWsBar :: String
myWsBar = "~/.local/bin/xmobar -f \"xft:Migu 1M:Regular:size=10\" ~/.xmobarrc"

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ wsPP { ppOutput = hPutStrLn h }

wsPP :: PP
wsPP = xmobarPP
    { ppOrder           = \(ws:l:t:_) -> [ws, t]
    , ppCurrent         = xmobarColor colorGreen colorNormalbg . wrap "ws" ""
    , ppUrgent          = xmobarColor colorWhite colorNormalbg
    , ppVisible         = xmobarColor colorWhite colorNormalbg
    , ppHidden          = xmobarColor colorWhite colorNormalbg
    , ppHiddenNoWindows = xmobarColor colorGray  colorNormalbg
    , ppTitle           = xmobarColor colorWhite colorNormalbg . takeStringJP 20
    , ppOutput          = putStrLn
    , ppWsSep           = xmobarColor colorNormalbg colorNormalbg " "
    , ppSep             = " ::: "
    }

-- ascii char : 1
-- others     : 2
charSize :: Char -> Int
charSize c
    | Char.isAscii c = 1
    | otherwise      = 2

-- take for japanese string
takeStringJP :: Int -> String -> String
takeStringJP _ ""  = ""
takeStringJP n str = take n' str where
    n' = last . L.findIndices (<= n) . scanl (+) 0 $ map charSize str

-- color****** :: String
colorBlue      = "#857da9"
colorGreen     = "#88b986"
colorGray      = "#676767"
colorWhite     = "#d3d7cf"
colorGrayAlt   = "#313131"
colorNormalbg  = "#1a1e1b"

