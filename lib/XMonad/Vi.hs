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

