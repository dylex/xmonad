{-# OPTIONS -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Global where

import XMonad as X hiding (terminal)
import Data.Ix
import Data.List
import Util

hostIs :: String -> Bool
hostIs s = case stripPrefix s hostName of
  Just "" -> True
  Just ('.':_) -> True
  _ -> False

hostHome :: Bool
hostHome = hostIs "datura"

newtype Desktop = Desktop { unDesktop :: Int } deriving (Eq, Ord, Enum, Ix)
instance Show Desktop where showsPrec n = showsPrec n . unDesktop
instance Read Desktop where readsPrec n = map (first Desktop) . readsPrec n
instance Bounded Desktop where
  minBound = Desktop 0
  maxBound = Desktop 7

desktops :: [Desktop]
desktops = allOf

desktopIds :: [WorkspaceId]
desktopIds = map show desktops -- assumed to be sorted

topHeight :: Int
topHeight = 50

wmod :: KeyMask
wmod = mod1Mask -- mod4Mask

colorFG :: COLOR
colorFG = "#BBBBA4"

colorBG :: COLOR
colorBG = "#8080AA"

colorRootFG :: COLOR
colorRootFG = "#FFFFBB"
