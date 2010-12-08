{-# LANGUAGE PatternGuards, FlexibleContexts #-}
module Ops
  ( debug
  , isElem
  , shellEscape
  , Run(..), unRun, run
  , spawnp, spawnl
  , warpFocus
  , stickWindow
  , switchWindow
  , viewDesk, shiftDesk
  , succWrap, predWrap
  , rotUp', rotDown'
  , floatAdjust
  ) where

import Control.Monad.Trans
import Data.Maybe
import System.IO.Unsafe
import XMonad as X
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.CopyWindow as XCW
import qualified XMonad.Actions.Warp as XWarp
import qualified XMonad.Util.Run as XRun
import Util
import Param
import Layout

debug :: Show a => a -> a
debug x = unsafePerformIO (trace (show x)) `seq` x

isElem :: Eq a => Query a -> [a] -> Query Bool
isElem q l = fmap (`elem` l) q

shellEscape :: String -> String
shellEscape "" = ""
shellEscape (c:s) 
  | c `elem` " !\"#$&'()*;<>?@[\\]`{|}" = '\\':c:shellEscape s
  | otherwise = c:shellEscape s

data Run 
  = Run String [String]
  | RunShell String

unRun :: Run -> [String]
unRun (Run p a) = p:a
unRun (RunShell c) = ["sh","-c",c]

run :: MonadIO m => Run -> m ()
run (Run p a) = XRun.safeSpawn p a
run (RunShell c) = XRun.unsafeSpawn c

spawnp :: MonadIO m => String -> m ()
spawnp p = run $ Run p []

spawnl :: MonadIO m => [String] -> m ()
spawnl [] = nop
spawnl (p:a) = run $ Run p a

warpFocus :: X ()
warpFocus = XWarp.warpToWindow 0.5 0.5

stickWindow :: Window -> WindowSet -> WindowSet
stickWindow w s = foldr (XCW.copyWindow w) s $ map show desktops

switchWindow :: Window -> X ()
switchWindow = sendMessage . SwitchWindow

withDesk :: (WorkspaceId -> WindowSet -> WindowSet) -> (Desktop -> Desktop) -> WindowSet -> WindowSet
withDesk f g ws = f (show $ g $ fromMaybe minBound $ readMaybe $ W.currentTag ws) ws

viewDesk :: (Desktop -> Desktop) -> WindowSet -> WindowSet
viewDesk = withDesk W.greedyView

shiftDesk :: (Desktop -> Desktop) -> WindowSet -> WindowSet
shiftDesk = withDesk W.shift
  
succWrap, predWrap :: (Enum a, Eq a, Bounded a) => a -> a
succWrap x
  | x == maxBound = minBound
  | otherwise = succ x
predWrap x
  | x == minBound = maxBound
  | otherwise = pred x

rotDown', rotUp' :: W.Stack a -> W.Stack a
rotDown' s@(W.Stack _ [] []) = s
rotDown' (W.Stack f (t:u) d) = W.Stack t u  (f:d) -- focusUp'
rotDown' (W.Stack f [] d)    = W.Stack t [] (f:d') where (d',t) = initLast d
rotUp' s@(W.Stack _ [] [])  = s
rotUp' (W.Stack f [] (t:d)) = W.Stack t []    (d++:f)
rotUp' (W.Stack f u [])    = W.Stack m (f:u') [] where (u',m) = initLast u
rotUp' (W.Stack f u (t:d)) = W.Stack t (f:u') (d++:m) where (u',m) = initLast u

windowHintAdjust :: Window -> X ()
windowHintAdjust w = withDisplay $ \dpy -> io $ do
  wa <- getWindowAttributes dpy w
  sh <- getWMNormalHints dpy w
  -- ideally would like width and height hints, but not included in SizeHints...
  resizeWindow dpy w `uncurry` 
    applySizeHints (ii $ wa_border_width wa) sh (wa_width wa, wa_height wa)

floatAdjust :: Window -> X ()
floatAdjust w = windowHintAdjust w >> float w
