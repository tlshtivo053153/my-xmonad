module Main where

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

baseConfig :: XConfig (ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full)))
baseConfig = desktopConfig
  { terminal        = myTerminal
  , modMask         = myModMask
  , workspaces      = myWorkspaces
  , manageHook      = myManageHook
  , layoutHook      = myLayout
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

