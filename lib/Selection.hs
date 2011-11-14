{-# OPTIONS -Wall #-}
module Selection
  ( withSelection
  , selectionEventHook
  ) where

import Data.IORef
import Data.Monoid
import Foreign.C.String (castCCharToChar)
import XMonad as X
import Util
import Ops

-- based on 'XMonad.Util.XSelection.getSelection' and xclip

getSelectionAtom :: X Atom
getSelectionAtom = getAtomCached $ globalAtomCache "XMONAD_SELECTION"

selectionAction :: IORef (Maybe (String -> X ()))
selectionAction = globalIORef Nothing

selectionEventHook :: Event -> X All
selectionEventHook (PropertyEvent{ ev_window = ew, ev_atom = ea, ev_propstate = es }) | es == propertyNewValue = do
  sa <- getSelectionAtom
  root <- asks theRoot
  if ew == root && ea == sa
    then io (readIORef selectionAction)
      >>= maybe (return (All True)) (\f -> do
	io $ writeIORef selectionAction Nothing
	s <- withDisplay $ \d -> io $ getWindowProperty8 d sa root
	maybe nop (f . map castCCharToChar) s
	return (All False))
    else return (All True)
selectionEventHook _ = return (All True)

withSelection :: (String -> X ()) -> X ()
withSelection a = do
  dpy <- asks display
  root <- asks theRoot
  clp <- getSelectionAtom
  io $ do
  writeIORef selectionAction (Just a)
  xConvertSelection dpy pRIMARY sTRING clp root currentTime
