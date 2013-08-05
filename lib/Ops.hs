{-# LANGUAGE FlexibleContexts #-}
module Ops
  ( debug
  , isElem
  , shellEscape
  , Run(..), unRun
  , run, runPipe
  , runInput, runOutput, runIO
  , AtomCache, globalAtomCache, getAtomCached
  , warpFocus
  , stickWindow
  , switchWindow
  , viewDesk, shiftDesk
  , succWrap, predWrap
  , rotUp', rotDown'
  , floatAdjust
  ) where

import Control.Monad
import Control.Monad.Trans
import Data.IORef
import Data.Maybe
import System.IO
import System.IO.Unsafe
import System.Posix.Process (executeFile, forkProcess, createSession)
import System.Process
import XMonad as X
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.CopyWindow as XCW
import qualified XMonad.Actions.Warp as XWarp
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

_runCmdSpec :: Run -> CmdSpec
_runCmdSpec (Run p a) = RawCommand p a
_runCmdSpec (RunShell s) = ShellCommand s

runCreateProcess :: Run -> CreateProcess
runCreateProcess (Run p a) = proc p a
runCreateProcess (RunShell s) = shell s

run :: MonadIO m => Run -> m ()
-- run r = io $ void $ createProcess $ runCreateProcess r -- throws?
run (RunShell c) = run $ Run "/bin/sh" ["-c", c]
run (Run p a) = io $ void $ forkProcess $ do
  uninstallSignalHandlers
  _ <- createSession
  executeFile p (head p /= '/') a Nothing

runPipe :: Run -> IO Handle
runPipe r = do
  (Just h, Nothing, Nothing, _) <- createProcess (runCreateProcess r){ std_in = CreatePipe }
  return h

runInput :: Run -> String -> IO ()
runInput r i = do
  hi <- runPipe r
  hPutStr hi i
  hClose hi

readAll :: Handle -> IO String
readAll h = do
  -- FIXME: this is both wrong and wrong
  o <- hGetContents h
  o `seq` hClose h
  return o

runOutput :: Run -> IO String
runOutput r = do
  (Nothing, Just ho, Nothing, _) <- createProcess (runCreateProcess r){ std_out = CreatePipe }
  readAll ho

runIO :: Run -> String -> IO String
runIO r i = do
  (Just hi, Just ho, Nothing, _) <- createProcess (runCreateProcess r){ std_in = CreatePipe, std_out = CreatePipe }
  hPutStr hi i
  hClose hi
  readAll ho

type AtomCache = (String, IORef Atom)

globalAtomCache :: String -> AtomCache
globalAtomCache s = (s, globalIORef 0)

getAtomCached :: AtomCache -> X Atom
getAtomCached (s,r) = do
  a <- io $ readIORef r
  if a == 0
    then withDisplay $ \d -> io $ do
      trace ("cacheAtom " ++ s)
      a' <- internAtom d s False
      writeIORef r a'
      return a'
    else return a

warpFocus :: X ()
warpFocus = XWarp.warpToWindow 0.5 0.5

stickWindow :: Window -> WindowSet -> WindowSet
stickWindow w s = foldr (XCW.copyWindow w . show) s (init desktops)

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
