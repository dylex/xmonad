{-# OPTIONS -Wall #-}
{-# LANGUAGE PatternGuards, ForeignFunctionInterface #-}
module Util 
  ( first, second
  , ii
  , nop
  , (>.), (>.=), (>=.)
  , (=.<), (.=<)
  , guard1
  , swap
  , rangeOf, allOf
  , deleteOne
  , partitionElems, partitionM
  , concatMapM
  , Nullable(..)

  , isElem
  , hostName
  , home
  , isExec
  , shellEscape
  , Run(..), unRun, run
  , spawnp, spawnl
  , warpFocus
  , COLOR
  , debug
  ) where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import qualified Foreign.C as C
import qualified Foreign.Marshal.Array
import qualified System.Directory
import System.Environment
import System.IO.Unsafe
import XMonad.Core as X
import qualified XMonad.Actions.Warp as XWarp
import qualified XMonad.Util.Run as XRun

ii :: (Integral a, Integral b) => a -> b
ii = fromIntegral

nop :: Monad m => m ()
nop = return ()

infixl 1 >., >.=, >=.
infixr 1 =.<, .=<
(>.) :: Monad m => m a -> b -> m b
(>.=) :: Monad m => m a -> (a -> b) -> m b
(=.<) :: Monad m => (a -> b) -> m a -> m b
(>=.) :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
(.=<) :: Monad m => (b -> c) -> (a -> m b) -> a -> m c

(>.) e r = e >> return r
(>.=) e r = e >>= return . r
(=.<) r e = return . r =<< e
(>=.) e r = e >=> return . r
(.=<) r e = return . r <=< e

guard1 :: MonadPlus m => Bool -> a -> m a
guard1 True = return
guard1 False = const mzero

swap :: (a,b) -> (b,a)
swap    (x,y) =  (y,x)

rangeOf :: (Bounded a) => (a,a)
rangeOf = (minBound, maxBound)

allOf :: (Enum a, Bounded a) => [a]
allOf = enumFromTo minBound maxBound

deleteOne :: Eq a => a -> [a] -> Maybe [a]
deleteOne _ [] = Nothing
deleteOne x (y:l)
  | x == y = Just l
  | otherwise = fmap (y:) $ deleteOne x l

partitionElems :: Eq a => [a] -> [a] -> ([a], [a])
partitionElems _ [] = ([], [])
partitionElems [] l = ([], l)
partitionElems s (x:l)
  | Just s' <- deleteOne x s = first (x:) $ partitionElems s' l
  | otherwise               = second (x:) $ partitionElems s l

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM f (x:l) = do
  m <- f x
  (if m then first else second) (x:) =.< partitionM f l

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM _ [] = return []
concatMapM f (x:l) = liftM2 (++) (f x) $ concatMapM f l

-- like Monoid but useful
class Nullable a where
  nnull :: a
  isNull :: a -> Bool
  coalesce :: a -> a -> a
  coalesce x 
    | isNull x = id
    | otherwise = const x

instance Nullable (Maybe a) where
  nnull = Nothing
  isNull = isNothing

instance Nullable [a] where
  nnull = []
  isNull = null

isElem :: Eq a => Query a -> [a] -> Query Bool
isElem q l = fmap (`elem` l) q

foreign import ccall unsafe "gethostname" c_gethostname :: C.CString -> C.CSize -> IO C.CInt
getHostName :: IO String
getHostName = Foreign.Marshal.Array.allocaArray0 len $ \s -> do
  C.throwErrnoIfMinus1_ "getHostName" $ c_gethostname s (fromIntegral len)
  C.peekCString s
  where len = 256

hostName :: String
hostName = unsafePerformIO getHostName

home :: String
home = unsafePerformIO $ getEnv "HOME"

isExec :: String -> Bool
isExec = unsafePerformIO . (liftM isJust) . System.Directory.findExecutable 

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

type COLOR = String

debug :: Show a => a -> a
debug x = unsafePerformIO (trace (show x)) `seq` x
