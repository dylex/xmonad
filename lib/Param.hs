{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Param
  ( osName
  , hostName, hostHome
  , home
  , Desktop, desktops
  , iconDesktop, desktopsAll
  , topHeight
  , wmod
  , COLOR
  , colorRootFG
  , pagerWidth, pagerDeskWidth
  , isExec
  , isFont
  ) where

import XMonad as X hiding (terminal)
import Data.Ix
import Data.Maybe
import qualified System.Directory
import System.Environment
import System.IO.Unsafe
import System.Posix.Unistd
import Util

systemID :: SystemID
systemID = unsafePerformIO getSystemID

osName :: String
osName = systemName systemID

hostName :: String
hostName = takeWhile (/= '.') $ nodeName systemID

hostHome :: Bool
hostHome = hostName == "datura"

home :: String
home = unsafePerformIO $ getEnv "HOME"

newtype Desktop = Desktop { _unDesktop :: Int } deriving (Eq, Ord, Enum, Ix)

instance Bounded Desktop where
  minBound = Desktop 0
  maxBound = Desktop 7

iconDesktop :: Desktop
iconDesktop = Desktop (-1)

desktops, desktopsAll :: [Desktop]
desktops = allOf
desktopsAll = allOf ++ [iconDesktop]

instance Show Desktop where 
  show (Desktop (-1)) = "icon"
  show (Desktop n) = show n

instance Read Desktop where 
  readsPrec n s = [(iconDesktop, r) | ("icon", r) <- lex s] 
    ++ map (first Desktop) (readsPrec n s)

topHeight :: Int
topHeight = 50

wmod :: KeyMask
wmod = mod4Mask -- mod1Mask

type COLOR = String

colorRootFG :: COLOR
colorRootFG = "#FFFFBB"

pagerDeskWidth :: Int
pagerDeskWidth = 75

pagerWidth :: Int
pagerWidth = pagerDeskWidth*length desktops

isExec :: String -> Bool
isExec = unsafePerformIO . (isJust .=< System.Directory.findExecutable)

isFont :: String -> X Bool
isFont f = withDisplay $ \dpy -> io $ catch 
  (loadQueryFont dpy f >>= freeFont dpy >. True)
  (const $ return False)

