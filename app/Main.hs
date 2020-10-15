module Main where

import XMonad
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Fullscreen (fullscreenManageHook, fullscreenEventHook, FullscreenFull)
import XMonad.Config.Desktop

import XMonad.Hooks.ManageDocks ( AvoidStruts )
import XMonad.Util.Run(spawnPipe)

import qualified XMonad.Vim as Vim

import MyConfig.ManageHook
import MyConfig.Layout
import MyConfig.Xmobar
import MyConfig.Keybind

baseConfig :: XConfig
  (Choose
     (ModifiedLayout AvoidStruts (Choose Tall (Mirror Tall)))
     (ModifiedLayout FullscreenFull (ModifiedLayout AvoidStruts Full)))
baseConfig = desktopConfig
  { terminal        = myTerminal
  , modMask         = myModMask
  , workspaces      = myWorkspaces
  , manageHook      = myManageHook <+> fullscreenManageHook
  , layoutHook      = myLayout
  , handleEventHook = handleEventHook desktopConfig <+> fullscreenEventHook
  }

main :: IO ()
main = do
    wsbar <- spawnPipe myWsBar
    let conf = baseConfig { logHook = myLogHook wsbar <+> logHook baseConfig }
    Vim.xmonadVim conf myVimKeys

myTerminal :: String
myTerminal = "lxterminal"

myModMask :: KeyMask
myModMask = mod4Mask

myWorkspaces :: [String]
myWorkspaces = map show [1..9]

