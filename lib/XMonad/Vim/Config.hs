module XMonad.Vim.Config
( newVimConfig
, handleEventHookVim
, startupHookVim
, grabKeys
, changeKeys
, ungrabKeyAll
, grabKeyboardVim
) where

import XMonad

import XMonad.Vim.Core
import XMonad.Vim.UI
import XMonad.Vim.Parse.Key ( parseKeymap )
import XMonad.Vim.CommandAction ( defaultCommand )

import qualified Data.Map as M
import Data.Bits ( (.|.), (.&.), setBit )
import Data.Maybe (catMaybes)
import Control.Arrow (first)

import Control.Concurrent.MVar (newEmptyMVar)

import Data.Monoid (All(..))
import Control.Monad (when, unless, forM, forM_, void, join)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (modify, gets)
import Control.Monad.Reader (asks)
import Control.Concurrent (forkOS)

import XMonad.Core (X, XConfig(..))
import Graphics.X11 (KeyMask, KeySym)

import qualified XMonad.Vim.UI as UI
import qualified XMonad.Vim.UI.StatusBar as UIS
import qualified XMonad.Vim.UI.CommandLine as UIC
import qualified XMonad.Vim.Parse.Command as PC

--import qualified Graphics.UI.Gtk as Gtk
import qualified GI.Gtk as Gtk

newVimConfig :: KeyMask -> VimKeys -> IO VimConfig
newVimConfig mask vimKeys = do
  --statusBar <- Gtk.windowNewPopup
  --statusBarLabel <- Gtk.labelNew (Nothing :: Maybe String)
  --Gtk.labelSetMarkup statusBarLabel "<big>=== Normal ===</big>"
  --Gtk.containerAdd statusBar statusBarLabel
  --commandLine <- Gtk.windowNew
  commandLineConfig_ <- UIC.newCompleteStatus
  statusBarConfig_ <- UIS.newStatusBarConfig
  return VimConfig
    { vimModMask = mask
    --, statusBarWin = statusBar
    --, statusBarDefaultPosition = BottomRight
    --, commandLineWin = commandLine
    --, commandLineDefaultPosition = BottomRight
    , insertKeys  = mkVimTree . parseKeymap mask $ iKey vimKeys
    , normalKeys  = mkVimTree . parseKeymap mask $ nKey vimKeys
    , commandKeys = mkVimTree . parseKeymap mask $ cKey vimKeys
    , changeModeHook = changeModeHook_
    , postActionHook = postActionHook_
    , stackKeyHook = stackKeyHook_
    , statusBarConfig = statusBarConfig_
    , commandLineConfig = commandLineConfig_
    , commandList = commandList_
    }

handleEventHookVim :: (VimState -> IO ()) -> VimConfig -> IO VimState -> Event -> X All
-- for to switch tty
handleEventHookVim updateState c s e@MappingNotifyEvent{} = do
    io $ refreshKeyboardMapping e
    when (ev_request e `elem` [mappingKeyboard, mappingModifier]) $ do
        trace "handleEventHookVim"
        setNumlockMask
        runVim' updateState c s (changeKeys >> join (asks postActionHook) )
    return $ All False
handleEventHookVim updateState vc vs KeyEvent {ev_event_type = t, ev_state = m, ev_keycode = code}
    | t == keyPress = withDisplay $ \dpy -> do
        s  <- io $ keycodeToKeysym dpy code 0
        mClean <- cleanMask m
        runVim' updateState vc vs $ keyPressVim (mClean, s)
        return $ All False
handleEventHookVim _ _ _ _  = return $ All True

-- same as XMonad.Main.hs
setNumlockMask :: X ()
setNumlockMask = do
    dpy <- asks display
    ms <- io $ getModifierMapping dpy
    xs <- sequence [ do
                        ks <- io $ keycodeToKeysym dpy kc 0
                        if ks == xK_Num_Lock
                            then return (setBit 0 (fromIntegral m))
                            else return (0 :: KeyMask)
                        | (m, kcs) <- ms, kc <- kcs, kc /= 0]
    modify (\s -> s { numberlockMask = foldr (.|.) 0 xs })

startupHookVim :: (VimState -> IO ()) -> VimConfig  -> IO VimState -> X ()
startupHookVim updateState c s = do
  io $ UIS.runStatusBarAction' (statusBarConfig c) UIS.initStatusBarAction
  io $ UIC.runCompletion' (commandLineConfig c) UIC.initCompletion
  runVim' updateState c s $ do
      join $ asks changeModeHook <*> return Insert <*> gets mode
      join $ asks postActionHook
  io $ forkOS mainLoop
  return ()

grabKeys :: [(KeyMask, KeySym)] -> VimAction ()
grabKeys keys = do
  XConf { display = dpy, theRoot = rootw } <- xToVim ask
  let grab kc m = io $ grabKey dpy kc m rootw True grabModeAsync grabModeAsync
  keysymToKeycodes <- xToVim keysymToKeycodes_
  xToVim $ forM_ keys $ \(mask,sym) ->
       forM_ (keysymToKeycodes sym) $ \kc ->
            mapM_ (grab kc . (mask .|.)) =<< extraModifiers

keysymToKeycodes_ :: X (KeySym -> [KeyCode])
keysymToKeycodes_ = do
  XConf { display = dpy } <- ask
  let (minCode, maxCode) = displayKeycodes dpy
      allCodes = [fromIntegral minCode .. fromIntegral maxCode]
  syms <- forM allCodes $ \code -> io (keycodeToKeysym dpy code 0)
  let keysymMap = M.fromListWith (++) (zip syms [[code] | code <- allCodes])
      keysymToKeycodes sym = M.findWithDefault [] sym keysymMap
  return keysymToKeycodes

ungrabKeyAll :: X ()
ungrabKeyAll = do
    XConf { display = dpy, theRoot = rootw } <- ask
    io $ ungrabKey dpy anyKey anyModifier rootw

changeKeys :: VimAction ()
changeKeys = do
    keys <- do
      m <- gets mode
      asks $ case m of
              Insert  -> insertKeys
              Normal  -> normalKeys
              Command -> commandKeys
    xToVim ungrabKeyAll
    grabKeys $ headVimTree keys

grabKeyboardX :: X ()
grabKeyboardX = do
    XConf { display = dpy, theRoot = rootw } <- ask
    liftIO $ grabKeyboard dpy rootw False grabModeAsync grabModeAsync currentTime
    return ()

grabKeyboardVim :: VimAction ()
grabKeyboardVim = do
    grab <- gets isGrab
    unless grab $ do
      xToVim grabKeyboardX
      modify (\s -> s { isGrab = True })

grabPointerX :: X ()
grabPointerX = do
    XConf { display = dpy, theRoot = rootw } <- ask
    liftIO $ grabPointer dpy rootw False buttonPressMask grabModeAsync grabModeAsync none none currentTime
    return ()

grabPointerVim :: VimAction ()
grabPointerVim = xToVim grabPointerX

ungrabKeyboardX :: X ()
ungrabKeyboardX = do
    XConf { display = dpy } <- ask
    liftIO $ ungrabKeyboard dpy currentTime

ungrabKeyboardVim :: VimAction ()
ungrabKeyboardVim = do
    grab <- gets isGrab
    when grab $ do
      xToVim ungrabKeyboardX
      modify (\s -> s { isGrab = False })

ungrabPointerX :: X ()
ungrabPointerX = do
    XConf { display = dpy } <- ask
    liftIO $ ungrabPointer dpy currentTime

ungrabPointerVim :: VimAction ()
ungrabPointerVim = xToVim ungrabPointerX

changeModeHook_ :: VimMode -> VimMode -> VimAction ()
changeModeHook_ before after | before /= after= do
    changeKeys
    case before of
      Normal  -> ungrabKeyboardVim >> ungrabPointerVim >> hideStatusBar
      Insert  -> return ()
      Command -> ungrabPointerVim >> hideCommandLine
    case after of
      Normal  -> grabKeyboardVim >> grabPointerVim
                 >> UI.writeStatusBarText "=== Normal ==="
                 >> showStatusBar
      Insert  -> return ()
      Command -> grabPointerVim >> showCommandLine
changeModeHook_ _ _ = return ()

postActionHook_ :: VimAction ()
postActionHook_ = do
    m <- gets mode
    c <- gets count
    case m of
        Normal ->
            if c == 0
            then UI.writeStatusBarText "=== Normal ==="
            else UI.writeStatusBarText $ show c
        Insert -> ungrabKeyboardVim
        Command -> ungrabKeyboardVim

--stackKeyHook_ :: VimAction ()
--stackKeyHook_ = grabKeyboardVim

stackKeyHook_ :: VimAction ()
stackKeyHook_ = do
    m <- gets mode
    c <- gets count
    case m of 
        Normal -> 
            let c' = if c == 0 then "" else show c
            in UI.writeStatusBarText =<< (c' ++) . unwords . reverse <$> gets stackKeyReadable
        Insert -> return ()
        Command -> return ()
    grabKeyboardVim

commandList_ :: VimAction ( [String] -> VimAction (), [PC.Command] )
commandList_ = defaultCommand

