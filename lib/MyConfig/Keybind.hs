module MyConfig.Keybind
( myVimKeys )
where

import System.Exit ( exitSuccess )
import Control.Arrow (first, second)

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS ( prevWS, nextWS, shiftToPrev, shiftToNext )

import qualified XMonad.Vim as Vim

myWindowOperation :: [(String, X ())]
myWindowOperation =
  map (first ("M1-C-" ++)) [
    ("h"  , prevWS )
  , ("l"  , nextWS )
  , ("<Right>", shiftToNext )
  , ("<Left>", shiftToPrev )
  , ("S-l", shiftToNext )
  , ("S-h", shiftToPrev )
  , ("j"  , windows W.focusDown )
  , ("k"  , windows W.focusUp   )
  , ("<Down>", windows W.swapDown )
  , ("<Up>", windows W.swapUp   )
  , ("S-j", windows W.swapDown )
  , ("S-k", windows W.swapUp   )
  ]

mySpawn :: [(String, X ())]
mySpawn =
  map (second spawn)
  [
    ("M-t", "lxterminal" )
  ]

myQuitWrite :: [(String, X ())]
myQuitWrite =
  [
    ("M-S-q", writeStateToFile >> io exitSuccess )
  , ("M-q",   spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
  ]

myDefaultKey :: [(String, X ())]
myDefaultKey =
  [ ("M-S-<Return>", spawn "lxterminal")
  , ("M-p", spawn "dmenu_run")
  , ("M-S-p", spawn "gmrun")
  , ("M-S-c", kill)
  , ("M-<space>", sendMessage NextLayout)
  , ("M-n", refresh)
  , ("M-<Tab>", windows W.focusDown)
  , ("M-S-<Tab>", windows W.focusUp)
  , ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)
  , ("M-m", windows W.focusMaster)
  , ("M-<Return>", windows W.swapMaster)
  , ("M-S-j", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)
  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)
  , ("M-,", sendMessage (IncMasterN 1))
  , ("M-.", sendMessage (IncMasterN (-1)))
  ]
  ++
  [ ("M-" ++ key, screenWorkspace sc >>= flip whenJust (windows . W.shift) )
    | (key, sc) <- zip ["w", "e", "r"] [0..]
  ]

myVimKeys :: Vim.VimKeys
myVimKeys = Vim.VimKeys {
  Vim.nKey =
    map (second Vim.ignoreCount)
    [ ("j", Vim.xToVim $ windows W.focusDown)
    , ("k", Vim.xToVim $ windows W.focusUp)
    , ("h", Vim.xToVim prevWS)
    , ("l", Vim.xToVim nextWS)
    , ("i", Vim.changeMode Vim.Insert)
    , ("y y", Vim.yankWindow '\"')
    , ("y w", Vim.yankWindowWS '\"')
    , ("p", Vim.pasteWindow '\"')
    , ("S-p", Vim.moveWindow '\"')
    , ("x", Vim.killSafeWindow )
    , ("S-x", Vim.killAllOtherCopyWindows )
    , ("C-x", Vim.kill1Window )
    , (":", Vim.callCommand "")
    , ("f", Vim.callCommand "focus ")
    ] ++
    [ (show c, Vim.counting c) | c <- [0..9] ]
    ++ map (second $ Vim.countAction . flip Vim.replicateAction)
    [ ("S-h", Vim.xToVim prevWS)
    , ("S-l", Vim.xToVim nextWS)
    ]
  , Vim.iKey = ("M-[", Vim.changeMode Vim.Normal) :
      map (second Vim.xToVim)
        (
        myDefaultKey
        ++ myWindowOperation
        ++ mySpawn
        ++ myQuitWrite
        )
  , Vim.cKey =
      [ ("C-[", Vim.cancelCommand )
      , ("<Return>", Vim.runCommand )
      , ("C-m", Vim.runCommand )
      , ("C-p", Vim.upCommandLineCursor )
      , ("C-n", Vim.downCommandLineCursor )
      , ("<Tab>", Vim.downCommandLineCursor )
      ]
    }

