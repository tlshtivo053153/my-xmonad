module XMonad.Vim.CommandAction
( defaultCommand
, myCommand
, focus
, bring
, goWS
, quit
, send
, restart
) where

import XMonad.Vim.Core

import XMonad hiding ( focus, restart )
import qualified XMonad.StackSet as W
import XMonad.Util.NamedWindows (getName)
import XMonad.Actions.WindowBringer (windowMap', bringWindow)


import System.Exit  ( exitSuccess )
import System.Environment ( getProgName )
import Data.List ( find )
import Data.Maybe ( fromMaybe )
import Text.Read ( readMaybe )

import qualified Data.Map as M
import Numeric (showHex, readHex)

import qualified XMonad.Vim.Parse.Command as PC

myCommand :: [( String, [PC.Command], [String] -> VimAction () )]
          -> VimAction ( [String] -> VimAction (), [PC.Command] )
myCommand mycs = do
    let f (x:xs) = maybe (return ()) (trd xs)
                   (find (\ (name, _, _) -> name == x) mycs)
    let cs = map (\(str, next, _) -> PC.Command str next) mycs
    return (f, cs)
      where
        trd xs (_, _, x) = x xs

defaultCommand :: VimAction ( [String] -> VimAction (), [PC.Command] )
defaultCommand = myCommand =<< sequence commands
      where
        commands = [focus, bring, goWS, quit, send, restart, progNameOut]

focus :: VimAction ( String, [PC.Command], [String] -> VimAction () )
focus = completeWSWindow "focus" W.focusWindow W.greedyView

bring :: VimAction ( String, [PC.Command], [String] -> VimAction () )
bring = completeWindow "bring" bringWindow

goWS :: VimAction ( String, [PC.Command], [String] -> VimAction () )
goWS = completeWorkspace "goWS" W.greedyView

quit :: VimAction ( String, [PC.Command], [String] -> VimAction () )
quit = return ("quit", [], const quitAction)
    where quitAction = xToVim $ writeStateToFile >> io exitSuccess

--yank :: VimAction ( String, [PC.Command], [String] -> VimAction () )
--yank = completeWindow "yank" undefined
--
--paste :: VimAction ( String, [PC.Command], [String] -> VimAction () )
--paste = undefined
--
--move :: VimAction ( String, [PC.Command], [String] -> VimAction () )
--move = undefined

send :: VimAction ( String, [PC.Command], [String] -> VimAction () )
send = return ("sendMessage", PC.toCommand $ map fst actions, f1 f)
    where
        actions = [ ("FirstLayout", sendMessage FirstLayout)
                  , ("NextLayout", sendMessage NextLayout)
                  , ("Shrink", sendMessage Shrink)
                  , ("Expand", sendMessage Expand)
                  , ("IncMaster1", sendMessage $ IncMasterN 1)
                  , ("IncMaster-1", sendMessage $ IncMasterN (-1))
                  ]
        f str = xToVim . fromMaybe (return ()) $ lookup str actions

restart :: VimAction ( String, [PC.Command], [String] -> VimAction () )
restart = return ("restart", [], const restartAction)
    where
      restartAction = spawn =<< (++ " --restart") <$> liftIO getProgName

progNameOut :: VimAction ( String, [PC.Command], [String] -> VimAction () )
progNameOut = return ("progNameOut", [], const progNameOutAction)
    where
      progNameOutAction = liftIO $ putStrLn =<< getProgName

bash :: VimAction ( String, [PC.Command], [String] -> VimAction () )
bash = undefined

zsh :: VimAction ( String, [PC.Command], [String] -> VimAction () )
zsh = undefined

completeWindow :: String -> (Window -> WindowSet -> WindowSet)
               -> VimAction ( String, [PC.Command], [String] -> VimAction () )
completeWindow name f = do
    command <- commandWindow
    return (name, command, windowAction f)

completeWorkspace :: String -> (String -> WindowSet -> WindowSet)
                  -> VimAction ( String, [PC.Command], [String] -> VimAction () )
completeWorkspace name f = do
    command <- commandWorkspace
    return (name, command, workspaceAction f)

completeWSWindow :: String -> (Window -> WindowSet -> WindowSet) -> (String -> WindowSet -> WindowSet)
                 -> VimAction ( String, [PC.Command], [String] -> VimAction () )
completeWSWindow name f g = do
    command <- commandWindow
    return (name, command, windowWSAction f g)

commandWorkspace :: VimAction [PC.Command]
commandWorkspace = xToVim . asks $ PC.toCommand . workspaces . config

commandWindow :: VimAction [PC.Command]
commandWindow = xToVim $ PC.toCommand . M.keys <$> uniqueNameMap

workspaceAction :: (String -> WindowSet -> WindowSet) -> [String] -> VimAction ()
workspaceAction f = f1 $ xToVim . windows . f

windowAction :: (Window -> WindowSet -> WindowSet) -> [String] -> VimAction ()
windowAction f (x:xs) = xToVim $ do
    win <- M.lookup x <$> uniqueNameMap
    whenJust win (windows . f)
windowAction _ _ = return ()

windowWSAction :: (Window -> WindowSet -> WindowSet) -> (String -> WindowSet -> WindowSet) -> [String] -> VimAction ()
windowWSAction f g xs = xToVim (windows . g $ getTag xs) >> windowAction f xs
    where
      getTag (str:_) = reverse $ takeWhile (/= '[') $ drop 1 $ dropWhile (/= ']') $ reverse str
      getTag _ = []

f0 :: VimAction () -> [String] -> VimAction ()
f0 f _ = f

f1 :: (String -> VimAction ()) -> [String] -> VimAction ()
f1 f (x:_) = f x
f1 _ _ = return ()

f2 :: (String -> String -> VimAction ()) -> [String] -> VimAction ()
f2 f (x:y:_) = f x y
f2 _ _ = return ()

decorateUniqueName :: WindowSpace -> Window -> X String
decorateUniqueName ws w = do
    name <- show <$> getName w
    return $  name ++ "[" ++ W.tag ws ++ "]" ++ showHex w ""

uniqueNameMap :: X (M.Map String Window)
uniqueNameMap = windowMap' decorateUniqueName

