{-# LANGUAGE PatternGuards, ForeignFunctionInterface, ScopedTypeVariables #-}
module Util 
  ( first, second
  , ii
  , nop
  , (>.), (>.=), (>=.)
  , (=.<), (.=<)
  , guard1
  , swap
  , rangeOf, allOf
  , (++:), initLast
  , deleteOne
  , partitionElems, partitionM
  , concatMapM
  , Nullable(..)
  , readMaybe
  , toEnumMaybe
  , globalIORef
  ) where

import Control.Arrow
import Control.Monad
import Data.IORef
import Data.Maybe
import GHC.List (errorEmptyList)
import System.IO.Unsafe

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

infixl 4 ++:
(++:) :: [a] -> a -> [a]
l ++: x = l ++ [x]

initLast :: [a] -> ([a], a)
initLast [x] = ([], x)
initLast (x:l) = first (x:) $ initLast l
initLast [] = errorEmptyList "initLast"

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

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x,"")] -> Just x
  _ -> Nothing

toEnumMaybe :: forall a . (Enum a, Bounded a) => Int -> Maybe a
toEnumMaybe x 
  | x >= fromEnum (minBound :: a) && x <= fromEnum (maxBound :: a) = Just (toEnum x)
  | otherwise = Nothing

globalIORef :: a -> IORef a
globalIORef = unsafePerformIO . newIORef
