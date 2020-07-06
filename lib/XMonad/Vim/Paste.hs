{-# OPTIONS_GHC -lXtst #-}

module XMonad.Vim.Paste (
  sendKey''',
  sendKeyWindow''',
  sendKey'''',
  sendKeyWindow''''
) where

import Data.Bits ( (.&.), testBit )

import Control.Concurrent (threadDelay)

import Foreign
import Foreign.C.Types
import Foreign.Marshal.Array

import XMonad (withDisplay, withFocused, io, theRoot, X, getEvent, Event(KeyEvent), XConf(..))
import Graphics.X11
import qualified Graphics.X11 as X11
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras (none, setEventType, setKeyEvent, currentTime)
import Control.Monad (forM_, when)
import qualified Control.Monad as M
import Control.Monad.Reader (ask, asks)
import Control.Monad.State ( get, gets, modify )
import Control.Monad.Fix ( fix )

import XMonad.Vim.Core ( VimAction, runVim, xToVim, VimConfig(..), VimState(..), VimMode(..) )
import XMonad.Vim.Config ( changeKeys, ungrabKeyAll, grabKeyboardVim )

foreign import ccall unsafe "X11/extensions/XTest.h XTestFakeKeyEvent"
    xTestFakeKeyEvent :: Display -> KeyCode -> Bool -> CULong -> IO Status
foreign import ccall unsafe "X11/extensions/XTest.h XTestGrabControl"
    xGrabControl :: Display -> Bool -> IO Status

queryKeymap :: Display -> IO [KeyCode]
queryKeymap display = do
    keys_return <- mallocArray 32 :: IO (Ptr CChar)
    xQueryKeymap display keys_return
    keysReturn <- peekArray 32 keys_return
    let pressingKeys = concatMap (\x -> map (x `testBit`) [0..7]) keysReturn
    return . map snd . filter fst $ zip pressingKeys [0..255]
foreign import ccall unsafe "X11/Xlib.h XQueryKeymap"
    xQueryKeymap :: Display -> Ptr CChar -> IO Int

withGrabbedControl :: Display -> IO a -> IO a
withGrabbedControl dpy action = do
    st <- xGrabControl dpy True
    if st /= 0 -- Grabbed successfully
      then do
           result <- action
           xGrabControl dpy False
           return result
      else fail "XTest cannot grab control"

sendKeyWindow' :: KeyMask -> KeySym -> Window -> X ()
sendKeyWindow' mods key w = withDisplay $ \dpy -> do
    let mods' = maskToSym mods
    rootw <- asks theRoot
    (_, _, _, _, _, _, _, pmask) <- io $ queryPointer dpy w
    let pmask' = maskToSym pmask
    --status <- io $ grabKeyboard dpy rootw True grabModeSync grabModeSync currentTime
    --io $ grabPointer dpy rootw False buttonPressMask grabModeAsync grabModeAsync none none currentTime
    --when (status == grabSuccess) . io $ do
    --io $ withGrabbedControl dpy $ do
    io $ do
      --forM_ pmask' $ \sym ->
      --  sendKeyEvent dpy w sym False
      --forM_ mods' $ \sym ->
      --  sendKeyEvent dpy w sym True
      grabKeyboard dpy rootw False grabModeSync grabModeSync currentTime
      sendKeyEvent dpy w key False
      sendKeyEvent dpy w xK_ISO_Level3_Shift True
      ungrabKeyboard dpy currentTime
      sendKeyEvent dpy w key True
      grabKeyboard dpy rootw False grabModeSync grabModeSync currentTime
      sendKeyEvent dpy w key False
      ungrabKeyboard dpy currentTime
      sendKeyEvent dpy w xK_ISO_Level3_Shift False
      --forM_ (reverse mods') $ \sym ->
      --  sendKeyEvent dpy w sym False
      --forM_ (reverse pmask') $ \sym ->
      --  sendKeyEvent dpy w sym True
      --ungrabPointer dpy currentTime
      --ungrabKeyboard dpy currentTime

sendKeyWindow'' :: KeyMask -> KeySym -> Window -> VimAction ()
sendKeyWindow'' mods key w = do
    XConf { theRoot = rootw , display = dpy } <- xToVim ask
    queryKeys <- io $ queryKeymap dpy
    xToVim ungrabKeyAll
    io $ do
      -- pre process
      --grabKeyboard dpy rootw False grabModeAsync grabModeAsync currentTime
      grabKeyboard dpy rootw False grabModeSync grabModeSync currentTime
      forM_ queryKeys $ \qkey ->
          sendKeyEvent' dpy w qkey False
      ungrabKeyboard dpy currentTime

      -- process
      let mods' = maskToSym mods
      forM_ mods' $ \sym ->
        sendKeyEvent dpy w sym True
      sendKeyEvent dpy w key True
      sendKeyEvent dpy w key False
      forM_ mods' $ \sym ->
        sendKeyEvent dpy w sym False

      -- post process
      --grabKeyboard dpy rootw False grabModeAsync grabModeAsync currentTime
      grabKeyboard dpy rootw False grabModeSync grabModeSync currentTime
      forM_ queryKeys $ \qkey ->
          sendKeyEvent' dpy w qkey True
      ungrabKeyboard dpy currentTime
      flush dpy
    changeKeys


sendKeyWindow''' :: [KeyCode] -> KeyMask -> KeySym -> Window -> VimAction ()
sendKeyWindow''' modMapKeyCodes mods key w = do
    XConf { theRoot = rootw , display = dpy } <- xToVim ask
    queryKeys <- io $ queryKeymap dpy
    xToVim ungrabKeyAll
    io $ do
      -- pre process
      --grabKeyboard dpy rootw False grabModeAsync grabModeAsync currentTime
      grabKeyboard dpy rootw False grabModeSync grabModeSync currentTime
      forM_ queryKeys $ \qkey ->
          sendKeyEvent' dpy w qkey False
      ungrabKeyboard dpy currentTime

      -- process
      let mods' = maskToSym mods
      forM_ mods' $ \sym ->
        sendKeyEvent dpy w sym True
      sendKeyEvent dpy w key True
      sendKeyEvent dpy w key False
      forM_ mods' $ \sym ->
        sendKeyEvent dpy w sym False

      -- post process
      --grabKeyboard dpy rootw False grabModeAsync grabModeAsync currentTime
      grabKeyboard dpy rootw False grabModeSync grabModeSync currentTime
      let queryKeys' = filter (`elem` modMapKeyCodes) queryKeys
      forM_ queryKeys' $ \qkey ->
          sendKeyEvent' dpy w qkey True
      flush dpy
      ungrabKeyboard dpy currentTime
    changeKeys

sendKeyWindow'''' :: [KeyCode] -> [KeyCode] -> KeySym -> Window -> VimAction ()
sendKeyWindow'''' modMapKeyCodes mods key w = do
    XConf { theRoot = rootw , display = dpy } <- xToVim ask
    queryKeys <- io $ queryKeymap dpy
    xToVim ungrabKeyAll
    grab <- gets isGrab
    io $ do
      -- pre process
      --grabKeyboard dpy rootw False grabModeAsync grabModeAsync currentTime
      grabKeyboard dpy rootw False grabModeSync grabModeSync currentTime
      forM_ queryKeys $ \qkey ->
          sendKeyEvent' dpy w qkey False
      ungrabKeyboard dpy currentTime

      -- process
      forM_ mods $ \sym ->
        sendKeyEvent' dpy w sym True
      sendKeyEvent dpy w key True
      sendKeyEvent dpy w key False
      forM_ mods $ \sym ->
        sendKeyEvent' dpy w sym False

      -- post process
      --grabKeyboard dpy rootw False grabModeAsync grabModeAsync currentTime
      grabKeyboard dpy rootw False grabModeSync grabModeSync currentTime
      let queryKeys' = filter (`elem` modMapKeyCodes) queryKeys
      forM_ queryKeys' $ \qkey ->
          sendKeyEvent' dpy w qkey True
      ungrabKeyboard dpy currentTime
      flush dpy
    changeKeys
    m <- gets mode
    case m of
        Normal -> io (grabKeyboard dpy rootw False grabModeAsync grabModeAsync currentTime)
                  >> modify (\s -> s { isGrab = True })
        Insert -> modify (\s -> s { isGrab = False } )
        Command -> modify (\s -> s { isGrab = False } )
    return ()

xK_ISO_Level3_Shift :: KeySym
xK_ISO_Level3_Shift = 0xfe03

--sendKeyWindow' :: KeyMask -> KeySym -> Window -> X ()
--sendKeyWindow' mods key w = withDisplay $ \dpy -> do
--    let mods' = maskToSym mods
--    rootw <- asks theRoot
--    keycode <- io $ keysymToKeycode dpy key
--    io $ withGrabbedControl dpy $ do
--      forM_ mods' $ \sym -> do
--        code <- keysymToKeycode dpy sym
--        xTestFakeKeyEvent dpy code True 0 --press
--      xTestFakeKeyEvent dpy keycode True 0 --press
--      xTestFakeKeyEvent dpy keycode False 0 --release
--      forM_ (reverse mods') $ \sym -> do
--        code <- keysymToKeycode dpy sym
--        xTestFakeKeyEvent dpy code False 0 --relese
--      X11.sync dpy False
--      return ()

--sendKey' = (withFocused .) . sendKeyWindow'

sendKey' :: KeyMask -> KeySym -> X ()
sendKey' = (withFocused .) . sendKeyWindow'
--sendKeyWindow' mods key w = withDisplay $ \d -> do
--    rootw <- asks theRoot
--    keycode <- io $ keysymToKeycode d key
--    io $ allocaXEvent $ \ev -> do
--        setEventType ev keyPress
--        setKeyEvent ev w rootw none mods keycode True 
--        sendEvent d w True keyPressMask ev
--        setEventType ev keyRelease
--        setKeyEvent ev w rootw none 0 0 True 
--        sendEvent d w True keyReleaseMask ev

sendKey'' :: KeyMask -> KeySym -> VimAction ()
sendKey'' mask sym = do
    vconfig <- ask
    vstate <- get
    xToVim $ do
        let send w = M.void $ runVim vconfig vstate (sendKeyWindow'' mask sym w)
        withFocused send

sendKey''' :: [KeyCode] -> KeyMask -> KeySym -> VimAction ()
sendKey''' modMapKeyCodes mask sym = do
    vconfig <- ask
    vstate <- get
    xToVim $ do
        let send w = M.void $ runVim vconfig vstate (sendKeyWindow''' modMapKeyCodes mask sym w)
        withFocused send

sendKey'''' :: [KeyCode] -> [KeyCode] -> KeySym -> VimAction ()
sendKey'''' modMapKeyCodes mask sym = do
    vconfig <- ask
    vstate <- get
    xToVim $ do
        let send w = M.void $ runVim vconfig vstate (sendKeyWindow'''' modMapKeyCodes mask sym w)
        withFocused send

maskToSym :: KeyMask -> [KeySym]
maskToSym mask = filter (/= 0) $ map f modMasks
  where
    f m = case mask .&. m of
      m'
        | m' == noModMask   -> 0
        | m' == shiftMask   -> xK_Shift_L
        | m' == lockMask    -> 0
        | m' == controlMask -> xK_Control_L
        | m' == mod1Mask    -> xK_Alt_L
        | m' == mod2Mask    -> xK_Num_Lock
        | m' == mod3Mask    -> 0
        | m' == mod4Mask    -> xK_Super_L
        | m' == mod5Mask    -> 0 --xK_Mode_switch
        | otherwise         -> 0

modMasks :: [KeyMask]
modMasks =
  [ noModMask
  , shiftMask
  , lockMask
  , controlMask
  , mod1Mask
  , mod2Mask
  , mod3Mask
  , mod4Mask
  , mod5Mask
  ]

sendKeyEvent :: Display -> Window -> KeySym -> Bool -> IO ()
sendKeyEvent dpy w sym isPress = do
  keycode <- keysymToKeycode dpy sym
  sendKeyEvent' dpy w keycode isPress

sendKeyEvent' :: Display -> Window -> KeyCode -> Bool -> IO ()
sendKeyEvent' dpy w code isPress = do
  (_, root, _, rootX, rootY, _, _, _) <- queryPointer dpy w
  warpPointer dpy 0 w 0 0 0 0 1 1
  flush dpy
  --
  xTestFakeKeyEvent dpy code isPress 0
  flush dpy
  --
  warpPointer dpy 0 root 0 0 0 0 (fromIntegral rootX) (fromIntegral rootY)
  flush dpy

fakeKeyPress :: Display -> KeySym -> IO ()
fakeKeyPress dpy sym = fakeKeyPress' dpy =<< keysymToKeycode dpy sym

fakeKeyPress' :: Display -> KeyCode -> IO ()
fakeKeyPress' dpy code = M.void $ xTestFakeKeyEvent dpy code True 0

fakeKeyRelease :: Display -> KeySym -> IO ()
fakeKeyRelease dpy sym = fakeKeyRelease' dpy =<< keysymToKeycode dpy sym

fakeKeyRelease' :: Display -> KeyCode -> IO ()
fakeKeyRelease' dpy code = M.void $ xTestFakeKeyEvent dpy code False 0

fakeKeyToggle :: Display -> [KeyCode] -> IO ()
fakeKeyToggle dpy codes = do
    qcodes <- queryKeymap dpy
    let codes' = filter (`elem` qcodes) codes
    if null codes'
        then forM_ codes (fakeKeyPress' dpy)
        else forM_ codes (fakeKeyRelease' dpy)

main :: IO ()
main = return ()

