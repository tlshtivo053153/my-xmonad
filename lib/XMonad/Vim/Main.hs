{-# LANGUAGE FlexibleContexts #-}

module XMonad.Vim.Main where

import XMonad

import XMonad.Vim.Core
import XMonad.Vim.Config
import XMonad.Vim.UI

import Data.IORef
import qualified Data.Map as M

xmonadVim :: (LayoutClass l Window, Read (l Window)) => XConfig l -> VimKeys -> IO ()
xmonadVim xconf vimKeys = do
    initGUI
    vstate <- newIORef def
    vconf <- newVimConfig (modMask xconf) vimKeys
    xmonad $ xconf
        { keys = const M.empty
        , handleEventHook = handleEventHookVim (writeIORef vstate) vconf (readIORef vstate)
                            <+> handleEventHook xconf
        , startupHook = startupHookVim (writeIORef vstate) vconf (readIORef vstate)
                        <+> startupHook xconf
        }

