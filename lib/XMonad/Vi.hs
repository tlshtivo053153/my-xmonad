module XMonad.Vi
( viModeP
) where

import qualified Data.Map as M
import Control.Arrow (second)

import Graphics.X11.Types (KeyMask, KeySym)

import XMonad.Core (X)
import XMonad.Config (def)
import XMonad.Operations (refresh)
import XMonad.Actions.Submap (submapDefaultWithKey)
import XMonad.Util.EZConfig (mkKeymap)

type KeybindP = [(String, X ())]

main :: IO ()
main = return ()

-- | normalMode : normal mode keybind                ( e.g. hjkl )
-- | insertMode : normal mode to insert mode keybind ( e.g. aio  )
viModeP :: KeybindP -> KeybindP -> X ()
viModeP normalMode insertMode = loop where
    loop :: X ()
    toLoop :: (String, X ()) -> (String, X ())
    undefVimKey :: (KeyMask, KeySym) -> X ()
    loop = submapDefaultWithKey undefVimKey . mkKeymap def $ map toLoop normalMode
    toLoop = second (\x -> refresh >> x >> loop)
    undefVimKey key = refresh >> M.findWithDefault loop key (mkKeymap def insertMode)

-- TODO:
--   macro (impossible); need multi thread? like keystroke logging
--   visual mode
--   text object
--   register
--   command line
--   mark
--   count operation
--     3j == jjj
--     10h == 10 times h
--   vim global state
--     register
--     count
--     mark
--     repeat action
--   vim config (reader monad)
--     normal mode keybind
--     insert mode keybind
--     count keybind (1-9 -> 0-9 -> 0-9 -> ...)
--     mark keybind (e.g. m)
--     repeat keybind (e.g. .)
--     hint keybind (e.g. f)
--     count action :: Int -> Action
--     search keybind (e.g. / n N)
--     yank keybind (e.g. yy)
--     select register keybind (e.g. ")
--   follow hint
--   repeat action
--   search
--   yank and paste
--   vim action

