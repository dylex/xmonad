module Server
  ( ServerCommand(..)
  , serverEventHook
  , serverCommandType
  , getServerCommandType
  ) where

import XMonad
import qualified XMonad.StackSet as W
import Data.Monoid (All(..))
import Util
import Param
import Ops
import Prompt

type ServerCall = [Int] -> X ()

data ServerCommand 
  = ServerCommandView -- workspace index
  | ServerCommandFocus -- window id
  | ServerCommandShift -- window id[,desktop id]
  | ServerCommandWindowMenu -- window id
  deriving (Show, Eq, Ord, Enum, Bounded)

serverCommand :: ServerCommand -> ServerCall
serverCommand ServerCommandView (d:_) = windows $ viewDesk $ const $ toEnum d
serverCommand ServerCommandFocus (w:_) = windows $ W.focusWindow $ ii w
serverCommand ServerCommandShift (w:d:_) = do
  ds <- maybe (withWindowSet $ return . W.currentTag) (return . show) (toEnumMaybe d :: Maybe Desktop)
  windows $ W.shiftWin ds $ ii w
serverCommand ServerCommandWindowMenu (w:_) = promptWindowOp $ ii w
serverCommand c a = trace $ "Bad arguments to " ++ show c ++ ": " ++ show a

serverCommandType :: String
serverCommandType = "XMONAD_COMMAND"

getServerCommandType :: X Atom
getServerCommandType = do
  d <- asks display
  io $ internAtom d serverCommandType False

serverEventHook :: Atom -> Event -> X All
serverEventHook ct (ClientMessageEvent{ ev_message_type = t, ev_data = cmd:args }) | t == ct = do
  case toEnumMaybe (ii cmd) of
    Just c -> serverCommand c (map ii args) >. All False
    _ -> trace ("Unknown " ++ serverCommandType ++ ": " ++ show cmd) >. All True
serverEventHook _ _ = return (All True)
