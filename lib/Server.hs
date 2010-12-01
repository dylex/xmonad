{-# OPTIONS -Wall #-}
module Server
  ( ServerCommand(..)
  , serverEventHook
  , serverCommandType
  , getServerCommandType
  ) where

import XMonad
import qualified XMonad.StackSet as W
import Data.Array
import Data.Monoid (All(..))
import Util
import Global

type ServerCall = [Int] -> X ()

data ServerCommand 
  = ServerCommandView -- [workspace index]
  | ServerCommandFocus -- [window id]
  deriving (Eq, Ord, Enum, Ix, Bounded)

commandFunctions :: Array ServerCommand ServerCall
commandFunctions = array rangeOf
  [(ServerCommandView,	\(i:_) -> windows $ W.view $ show $ Desktop i)
  ,(ServerCommandFocus,	\(i:_) -> windows $ W.focusWindow $ ii i)]

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

serverCommandType :: String
serverCommandType = "XMONAD_COMMAND"

getServerCommandType :: X Atom
getServerCommandType = do
  d <- asks display
  io $ internAtom d serverCommandType False

serverEventHook :: Atom -> Event -> X All
serverEventHook ct (ClientMessageEvent{ ev_message_type = t, ev_data = cmd:args }) | t == ct = do
  if inRange (both fromEnum $ bounds commandFunctions) icmd
    then (commandFunctions ! toEnum icmd) (map ii args) >. All False
    else trace ("Unknown " ++ serverCommandType ++ ": " ++ show cmd) >. All True
  where icmd = ii cmd
serverEventHook _ _ = return (All True)
