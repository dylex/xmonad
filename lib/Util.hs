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
  , getHostName
  ) where

import Control.Arrow
import Control.Monad
import Data.Maybe
import qualified Foreign.C as C
import qualified Foreign.Marshal.Array

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

foreign import ccall unsafe "gethostname" c_gethostname :: C.CString -> C.CSize -> IO C.CInt
getHostName :: IO String
getHostName = Foreign.Marshal.Array.allocaArray0 len $ \s -> do
  C.throwErrnoIfMinus1_ "getHostName" $ c_gethostname s (fromIntegral len)
  C.peekCString s
  where len = 256
