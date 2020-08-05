{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module XMonad.Vim.Core
( VimAction
, VimMode(..)
, VimTree
, VimState(..)
, VimConfig(..)
, VimKeys(..)
, Register
, Mark
, Position(..)
, VimKeyMapP
, xToVim
, runVim
, runVim'
, whenVim
, isNormal
, isInsert
, isCommand
, mkVimKeyMap
, mkKeymap'
, mkVimTree
, keyPressVim
, headVimTree
) where

import qualified Data.Map as M
import Data.Bits ( (.|.), (.&.) )
import Data.Maybe (catMaybes, fromMaybe)

import XMonad hiding (Position)

import Control.Arrow (second, (***), (&&&))

import Data.Monoid (All(..), (<>) )
import Control.Monad (liftM2, when, join, forM, forM_)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.State (StateT, MonadState, gets, modify, runStateT)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks)
import Control.Monad.IO.Class (MonadIO)
import Data.Default (Default(..))

import XMonad.Core (X, XConfig(..))
import XMonad.Util.EZConfig (mkKeymap)
import Graphics.X11 (KeyMask, KeySym, mod4Mask, ungrabKeyboard)

import XMonad.Vim.Parse.Key ( parseKeymap, keyToString )

import XMonad.Vim.UI.Utils ( Position(..) )

import qualified XMonad.Vim.UI.CommandLine as UIC
import qualified XMonad.Vim.UI.StatusBar as UIS
import qualified XMonad.Vim.Parse.Command as PC

import qualified GI.Gtk as Gtk

newtype VimAction a = VimAction (ReaderT VimConfig (StateT VimState X) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState VimState,
            MonadReader VimConfig)

instance Default a => Default (VimAction a) where
  def = return def

instance Semigroup a => Semigroup (VimAction a) where
    (<>) = liftM2 (<>)

instance Monoid a => Monoid (VimAction a) where
  mempty = return mempty
  mappend = liftM2 mappend

data VimMode = Insert | Normal | Command
  deriving (Show, Read, Eq)

newtype VimTree key action = VimTree (M.Map key (action, VimTree key action) )

type Register = Char
type Mark = Char
data VimState = VimState
  { count :: !Int
  , mode :: !VimMode
  , register :: !(M.Map Register String)
  , mark :: !(M.Map Mark String)
  , searchText :: !(Maybe String)
  , stackKey :: ![(KeyMask, KeySym)]
  , stackKeyReadable :: ![String]
  , lastAction :: !(VimAction ())
  , isGrab :: !Bool
  , commandLinePosition :: Position
  , statusBarPosition :: Position
  , statusBarText :: Maybe String
  }

instance Default VimState where
  def = VimState
    { count = 0
    , mode = Normal
    , register = M.empty
    , mark = M.empty
    , searchText = Nothing
    , stackKey = []
    , stackKeyReadable = []
    , lastAction = return ()
    , isGrab = False
    , commandLinePosition = BottomRight
    , statusBarPosition = BottomRight
    --, statusBarText = Just "=== Normal ==="
    , statusBarText = Nothing
    }

data VimConfig = VimConfig
  { vimModMask :: !KeyMask -- modmask overwrite XConfig
  , insertKeys  :: !( VimTree (KeyMask, KeySym) (VimAction ()) )
  , normalKeys  :: !( VimTree (KeyMask, KeySym) (VimAction ()) )
  , commandKeys :: !( VimTree (KeyMask, KeySym) (VimAction ()) )
  , changeModeHook :: !(VimMode -> VimMode -> VimAction ())
  , postActionHook :: !(VimAction ())
  , stackKeyHook :: !(VimAction ())
  , statusBarConfig :: UIS.StatusBarConfig
  , commandLineConfig :: UIC.CompleteStatus
  , commandList :: !( VimAction ( [String] -> VimAction (), [PC.Command] ) )
  }

data VimKeys = VimKeys
  { nKey :: ![(String, VimAction ()) ]
  , iKey :: ![(String, VimAction ()) ]
  , cKey :: ![(String, VimAction ()) ]
  } 
type VimKeyListP = [(String, VimAction ())]
type VimKeyList  = [( (KeyMask, KeySym), VimAction () )]
type VimKeyMapP = M.Map String (VimAction ())
type VimKeyMap  = M.Map (KeyMask, KeySym) (VimAction ())

xToVim :: X a -> VimAction a
xToVim = VimAction . lift . lift

runVim :: VimConfig -> VimState -> VimAction a -> X (a, VimState)
runVim c s (VimAction action) = runStateT (runReaderT action c) s

runVim' :: (VimState -> IO ())
        -> VimConfig -> IO VimState -> VimAction a -> X a
runVim' updateState c s v = do
    s' <- liftIO s
    (a, s'') <- runVim c s' v
    liftIO $ updateState s''
    return a

isNormal :: VimAction Bool
isNormal = do
  m <- gets mode
  let isN x = case x of
                Normal         -> True
                _              -> False
  return $ isN m

isInsert :: VimAction Bool
isInsert = do
  m <- gets mode
  let isI x = case x of
                Insert         -> True
                _              -> False
  return $ isI m

isCommand :: VimAction Bool
isCommand = do
  m <- gets mode
  let isC x = case x of
                Command         -> True
                _              -> False
  return $ isC m

isVimMode :: VimMode -> VimAction Bool
isVimMode vimMode = do
  m <- gets mode
  return $ m == vimMode

whenVim :: VimAction Bool -> VimAction () -> VimAction ()
whenVim vb action = do
  b <- vb
  when b action

mkVimKeyMap ::
  (VimState -> IO ()) -> VimConfig -> IO VimState -> VimKeyListP ->
  M.Map (KeyMask, KeySym) (X ())
mkVimKeyMap updateState c s = M.fromList
  . map ((&&&) id $ \key ->
    runVim' updateState c s $ keyPressVim key
  )
  . mkKeymap' (vimModMask c)

keyPressVim :: (KeyMask, KeySym) -> VimAction ()
keyPressVim key = do
    pushStackKey key
    (match, candidate) <- candidateVimAction
    if candidate
      then match >> join (asks stackKeyHook)
      else match >> join (asks stackKeyHook) >> postAction >> clearStackKey

-- "a a"
-- "a b" -> (noModMask, xK_a)
-- "a c"
-- 
-- "M-a a"
-- "M-a b" -> (mod4Mask, xK_a)
--
-- "M-S-a a"
-- "S-M-a b" -> (shiftMask & mod4Mask, xK_a)
--
mkKeymap' :: KeyMask -> [ (String, VimAction ()) ] -> [ (KeyMask, KeySym) ]
mkKeymap' mask = map (head . fst) . parseKeymap mask 

pushStackKey :: (KeyMask, KeySym) -> VimAction ()
pushStackKey key = do
    sk <- gets stackKey
    skr <- gets stackKeyReadable
    modify (\s -> s { stackKey = key:sk } )
    modify (\s -> s { stackKeyReadable = keyToString key : skr } )

clearStackKey :: VimAction ()
clearStackKey = do
    modify (\s -> s { stackKey = [] } )
    modify (\s -> s { stackKeyReadable = [] } )

-- keybind "a b c" --> [xK_a, xK_b, xK_c]
-- input "a"                       --> [xK_a]
-- input "a", input "b"            --> [xK_b, xK_a]
-- input "a", input "b", input "c" --> [xK_c, xK_b, xK_a]
candidateVimAction :: VimAction ( VimAction (), Bool )
candidateVimAction = do
    sk <- reverse <$> gets stackKey
    (match, candidate) <- takeVimTree sk <$> getKeys
    return (match, not $ nullVimTree candidate)

getKeys :: VimAction ( VimTree (KeyMask, KeySym) (VimAction ()) )
getKeys = do
    m <- gets mode
    asks $ case m of
      Insert  -> insertKeys
      Normal  -> normalKeys
      Command -> commandKeys

mkVimTree :: [( [(KeyMask, KeySym)], VimAction () )]
          -> VimTree (KeyMask, KeySym) (VimAction ())
mkVimTree [] = mkVimTree' []
mkVimTree vks = mkVimTree' vks'
    where
      vks' = M.toList $ M.filterWithKey (\key _ -> not (null key) ) $ M.fromList vks

mkVimTree' :: (Ord k, Monad a) => [( [k], a () )] -> VimTree k (a ())
mkVimTree' [] = emptyVimTree
mkVimTree' vks = VimTree keyMap
    where
      toAssoc (key:keys,action) = (key, [(keys, action)] )
      --  g.f :: [( [(KeyMask, KeySym)], VimAction () )] -> (VimAction (), VimTree)
      f = foldr accFunc (Nothing, [])
      g = fromMaybe (return ()) *** mkVimTree'
      accFunc ([], action) (_, acc) = (Just action, acc)
      accFunc x acc = second (x:) acc
      keyMap = M.map (g.f) $ M.fromListWith (++) (map toAssoc vks)

takeVimTree :: [(KeyMask, KeySym)]
            -> VimTree (KeyMask, KeySym) (VimAction ())
            -> (VimAction (), VimTree (KeyMask, KeySym) (VimAction ()) )
takeVimTree = takeVimTree'

takeVimTree' :: (Ord k, Monad a) =>
                [k]
             -> VimTree k (a ())
             -> (a (), VimTree k (a ()) )
takeVimTree' [] vtree = (return (), vtree)
takeVimTree' (vk:vks) (VimTree vtree) = maybe noAction f $ M.lookup vk vtree
    where
      noAction = (return (), emptyVimTree)
      f = case vks of
            [] -> id
            _  -> takeVimTree' vks . snd

headVimTree :: VimTree k a -> [k]
headVimTree (VimTree vtree) = M.keys vtree

emptyVimTree :: VimTree k a
emptyVimTree = VimTree M.empty

nullVimTree :: VimTree k a -> Bool
nullVimTree (VimTree vtree) = M.null vtree

postAction :: VimAction ()
postAction = join $ asks postActionHook

main :: IO ()
main = return ()

