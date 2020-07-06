module Main where
--------------------------------------------------------------------------------
-- import module                                                             {{{1
--------------------------------------------------------------------------------

import qualified Data.Map as M
import qualified Data.List as L
import System.IO                       -- for xmobar
import System.Exit  -- exitWith
import Control.Arrow (first, second)

import XMonad
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Config.Desktop
import qualified XMonad.StackSet as W  -- myManageHookShift  --use

import XMonad.Actions.CycleWS   --use
import XMonad.Actions.WindowBringer (bringWindow)
import XMonad.Actions.CopyWindow (copy, kill1, copyWindow)

import XMonad.Hooks.DynamicLog         -- for xmobar --use
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks        -- avoid xmobar area

import XMonad.Prompt (XPConfig(..), XPPosition(..))
import XMonad.Prompt.Window            -- pops up a prompt with window names
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Util.EZConfig            -- removeKeys, additionalKeys --use
import XMonad.Util.Run(spawnPipe)      -- spawnPipe, hPutStrLn --use

import XMonad.Util.Dmenu (dmenu)
import XMonad.Util.Paste (pasteString)

import qualified Data.Char as Char

-- local module
--import XMonad.Prompt.MultiCommand (multiCommandPrompt, mapToMultiCommand, MultiCommand(..), mcWindow, mcWindow', mcWorkspace )
--import XMonad.Prompt.PromptTest
--import XMonad.Prompt.CompleteX
import XMonad.Vi (viModeP)

import Data.IORef  --global state test

import Control.Concurrent
import Control.Concurrent.MVar
import Graphics.UI.Gtk hiding (Shrink, Expand, currentTime)

import qualified XMonad.Vim as Vim
import qualified Graphics.X11 as X11

--------------------------------------------------------------------------------
-- comment                                                                   {{{1
--------------------------------------------------------------------------------

-- |
-- | "PP" is "Pretty Printing".
-- | "XP" is "X Prompt".
-- |

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
    _ <- initGUI
    vstate <- newIORef def
    vconf <- Vim.newVimConfig mod4Mask myVimKeys
    ----------------------------------------------------------------------------
    -- Config                                                                {{{2
    ----------------------------------------------------------------------------
    wsbar <- spawnPipe myWsBar
    xmonad $ baseConfig
        { logHook         = myLogHook wsbar <+> logHook desktopConfig
        , keys = const M.empty
        , handleEventHook = Vim.handleEventHookVim (writeIORef vstate) vconf (readIORef vstate) <+> handleEventHook baseConfig
        , startupHook = (<+> startupHook baseConfig) $
                            Vim.startupHookVim (writeIORef vstate) vconf (readIORef vstate)
        }
    where
    ----------------------------------------------------------------------------
    -- Keymap: window operations                                             {{{2
    ----------------------------------------------------------------------------
    ----  M1:Mod1(Alt), C:Control
    myWindowOperation =
      map (first ("M1-C-" ++)) [
      -- Go to the next / previous workspace
        ("h"  , prevWS )
      , ("l"  , nextWS )

      -- Shift the focused window to the next / previous workspace
      , ("<Right>", shiftToNext )
      , ("<Left>", shiftToPrev )
      , ("S-l", shiftToNext )
      , ("S-h", shiftToPrev )

      -- Move the focus down / up
      , ("j"  , windows W.focusDown )
      , ("k"  , windows W.focusUp   )

      -- Swap the focused window down / up
      , ("<Down>", windows W.swapDown )
      , ("<Up>", windows W.swapUp   )
      , ("S-j", windows W.swapDown )
      , ("S-k", windows W.swapUp   )
      ]
    myWindowPrompt =
      [
      -- Search a window and focus into the window
        ("M-g", windowPrompt myXPConfig Goto allWindows )
      -- Search a window and bring to the curreent workspace
      , ("M-b", windowPrompt myXPConfig Bring allWindows )
      ]
    ------------------------------------------------------------------------ ---
    -- Keymap: custom commands                                               {{{2
    ----------------------------------------------------------------------------
    myCustomCommands =
      map (second spawn)
      [
        ("M-t", "lxterminal" )
      ] --  :: [(String, X ())]
    ----------------------------------------------------------------------------
    -- Keymap: quit and write state file                                     {{{2
    ----------------------------------------------------------------------------
    myQuitWrite =
      [
        ("M-S-q", writeStateToFile >> io exitSuccess )
      , ("M-q",   spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
      ]
    ----------------------------------------------------------------------------
    -- Keymap: vi mode                                                       {{{2
    ----------------------------------------------------------------------------
    myViMode =
      [
      -- normal mode like vi
        ( "M-[", viModeP viNormalMode viToInsertMode )
      ]
    viNormalMode =
      [ ("j", windows W.focusDown  )
      , ("k", windows W.focusUp    )
      , ("h", prevWS               )
      , ("l", nextWS               )

      , ("S-j", windows W.swapUp   )
      , ("S-k", windows W.swapDown )
      , ("S-h", shiftToPrev        )
      , ("S-l", shiftToNext        )

      , ("x", kill1)

      , (":", dmenu ["command", "mode", "hello world"] >> return () )

      , (",", sendMessage $ IncMasterN 1    )
      , (".", sendMessage $ IncMasterN (-1) )
      ] ++ (do
          (mask, func) <- zip ["", "S-"] [W.view, W.shift]
          (key, screen) <- zip ["w", "e", "r"] [1,2,3]
          return (mask ++ key
                 , screenWorkspace screen >>= flip whenJust (windows . func)
                 )
           )
    viToInsertMode =
      [ ("i", return () )
      ]
    ----------------------------------------------------------------------------
    -- Keymap: default keybind                                              {{{2
    ----------------------------------------------------------------------------
    myDefaultKey conf =
      [ ("M-S-<Return>", spawn $ terminal conf)
      , ("M-p", spawn "dmenu_run")
      , ("M-S-p", spawn "gmrun")
      , ("M-S-c", kill)
      , ("M-<space>", sendMessage NextLayout)
      --, ("M-S-<Space>", setLayout $ layoutHook conf)
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
      --, ("M-t", withFocused $ windows . W.sink)
      , ("M-,", sendMessage (IncMasterN 1))
      , ("M-.", sendMessage (IncMasterN (-1)))
      --, ("M-S-q", io (exitWith ExitSuccess))
      --, ("M-q", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
      ]
      ++
      [ ("M-" ++ key, screenWorkspace sc >>= flip whenJust (windows . W.shift) )
        | (key, sc) <- zip ["w", "e", "r"] [0..]
      ]
      --[ ("M" ++ m ++ k, windows $ f i) 
      --  | (i, k) <- zip (workspaces conf) (map show ['1'..'9'])
    ----------------------------------------------------------------------------
    -- Keymap: vim mode                                                      {{{2
    ----------------------------------------------------------------------------
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
        , ("a", Vim.sendKey''' allModKeyCode X11.noModMask X11.xK_b)
        , ("c", Vim.sendKey'''' allModKeyCode [] X11.xK_d)
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
            myDefaultKey baseConfig
            ++ myWindowOperation
            ++ myWindowPrompt
            ++ myCustomCommands
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
    ---------------------------------------------------------------------}}}

--------------------------------------------------------------------------------
-- terminal                                                                  {{{1
--------------------------------------------------------------------------------
myTerminal :: String
myTerminal = "lxterminal"


--------------------------------------------------------------------------------
-- layout                                                                    {{{1
--------------------------------------------------------------------------------
myLayout :: ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full)) a
myLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

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

--------------------------------------------------------------------------------
-- manage hook                                                               {{{1
--------------------------------------------------------------------------------
myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [className =? c       --> unfloat | c <- myNoFloatsC ]
    , [title =? "XMonadVimCommandLineWindow" --> doFloat ]
    ]
    where
        unfloat     = ask >>= doF . W.sink
        myNoFloatsC = ["Xephyr"]

--------------------------------------------------------------------------------
-- xmobar                                                                    {{{1
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- X Prompt Config                                                           {{{1
--------------------------------------------------------------------------------
myXPConfig :: XPConfig
myXPConfig = def
  { font              = "xft:Migu 1M:size=20:antialias=true"
  , alwaysHighlight   = False
  , height            = 35
  , promptBorderWidth = 0
  , autoComplete      = Nothing
  , position          = Bottom
  }

--------------------------------------------------------------------------------
-- color                                                                     {{{1
--------------------------------------------------------------------------------

-- color****** :: String
colorBlue      = "#857da9"
colorGreen     = "#88b986"
colorGray      = "#676767"
colorWhite     = "#d3d7cf"
colorGrayAlt   = "#313131"
colorNormalbg  = "#1a1e1b"

--------------------------------------------------------------------------------
-- modkey                                                                    {{{1
--------------------------------------------------------------------------------
shiftCode :: X11.KeyCode
shiftCode = 0x32

controlCode :: X11.KeyCode
controlCode = 0x69

mod1Code :: X11.KeyCode
mod1Code = 0x25

mod2Code :: X11.KeyCode
mod2Code = 0x4d

mod4Code :: X11.KeyCode
mod4Code = 0x40

mod5Code :: X11.KeyCode
mod5Code = 0x5c

allModKeyCode :: [X11.KeyCode]
allModKeyCode = [0x32,0x3e,0x69,0x85,0x25,0x42,0x6c,0xcd,0x4d,0x40,0x86,0xce,0xcf,0x5c,0xcb]

-----------------------------------------------------------------------------}}}

{-
 - vim: foldmethod=marker
 - vim: foldcolumn=3
 - vim: foldlevelstart=0
-}
