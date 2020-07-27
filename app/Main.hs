module Main where
--------------------------------------------------------------------------------
-- import module                                                             {{{1
--------------------------------------------------------------------------------

import qualified Data.Map as M

import XMonad
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Config.Desktop

import XMonad.Hooks.ManageDocks ( AvoidStruts, avoidStruts )
import XMonad.Util.Run(spawnPipe)

import Data.IORef

import qualified XMonad.Vim as Vim

import MyConfig.ManageHook
import MyConfig.Layout
import MyConfig.Xmobar
import MyConfig.Keybind

--------------------------------------------------------------------------------
-- baseConfig                                                                {{{1
--------------------------------------------------------------------------------
baseConfig :: XConfig (ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full)))
baseConfig = desktopConfig
  { terminal        = myTerminal
  , modMask         = myModMask
  , workspaces      = myWorkspaces
  , manageHook      = myManageHook
  , layoutHook      = myLayout
  }

--------------------------------------------------------------------------------
-- main                                                                      {{{1
--------------------------------------------------------------------------------
main :: IO ()
main = do
     _ <- Vim.initGUI
     vstate <- newIORef def
     vconf <- Vim.newVimConfig mod4Mask myVimKeys

     wsbar <- spawnPipe myWsBar
     xmonad $ baseConfig
         { logHook         = myLogHook wsbar <+> logHook desktopConfig
         , keys = const M.empty
         , handleEventHook = Vim.handleEventHookVim (writeIORef vstate) vconf (readIORef vstate) <+> handleEventHook baseConfig
         , startupHook = (<+> startupHook baseConfig) $
                             Vim.startupHookVim (writeIORef vstate) vconf (readIORef vstate)
         }

--------------------------------------------------------------------------------
-- terminal                                                                  {{{1
--------------------------------------------------------------------------------
myTerminal :: String
myTerminal = "lxterminal"

--------------------------------------------------------------------------------
-- modMask                                                                   {{{1
--------------------------------------------------------------------------------
myModMask :: KeyMask
myModMask = mod4Mask

--------------------------------------------------------------------------------
-- work spaces                                                               {{{1
--------------------------------------------------------------------------------
myWorkspaces :: [String]
myWorkspaces = map show [1..9]

-----------------------------------------------------------------------------}}}

{-
 - vim: foldmethod=marker
 - vim: foldcolumn=3
 - vim: foldlevelstart=0
-}
