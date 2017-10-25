{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
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

import           Control.Exception (catch, IOException)
import           Data.Ix (Ix)
import           Data.List (isPrefixOf)
import           Data.Maybe (isJust)
import qualified System.Directory
import           System.Environment (getEnv)
import           System.IO.Unsafe (unsafeDupablePerformIO)
import           System.Posix.Unistd (SystemID, getSystemID, systemName, nodeName)
import qualified XMonad as X

import           Util

systemID :: SystemID
systemID = unsafeDupablePerformIO getSystemID

osName :: String
osName = systemName systemID

hostName :: String
hostName = takeWhile (/= '.') $ nodeName systemID

hostHome :: Bool
hostHome = hostName == "datura"

home :: String
home = unsafeDupablePerformIO $ getEnv "HOME"

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

wmod :: X.KeyMask
wmod = X.mod4Mask -- mod1Mask

type COLOR = String

colorRootFG :: COLOR
colorRootFG = "#FFFFBB"

pagerDeskWidth :: Int
pagerDeskWidth | hdpi      = 140
               | otherwise = 75

pagerWidth :: Int
pagerWidth = pagerDeskWidth*length desktops

isExec :: String -> Bool
isExec = unsafeDupablePerformIO . (isJust .=< System.Directory.findExecutable)

isFont :: String -> X.X Bool
isFont f = X.withDisplay $ \dpy -> X.io $ catch 
  (X.loadQueryFont dpy f >>= X.freeFont dpy >. True)
  (\(_ :: IOException) -> return False)

