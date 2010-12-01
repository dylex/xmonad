{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts, DeriveDataTypeable, PatternGuards #-}
module Layout
  ( splitLayout
  , SwitchWindow(..)
  ) where

import XMonad as X
import XMonad.StackSet
import XMonad.Util.Types
import XMonad.Util.WindowProperties
import Data.Maybe
import Util

-- based on ComboP
data SplitLayout lm ls a = SplitLayout 
  { splitDirection :: !Direction2D
  , splitSize :: !Dimension
  , splitProp :: !Property
  , splitMain :: !(lm a)
  , splitSub :: !(ls a)
  , splitMains, splitSubs :: ![a]
  } deriving (Show, Read)

data SwitchWindow = SwitchWindow Window deriving (Show, Read, Typeable)
instance Message SwitchWindow

splitLayout :: (LayoutClass lm Window, LayoutClass ls Window) => (Direction2D, Dimension) -> Property -> lm Window -> ls Window -> SplitLayout lm ls Window
splitLayout (d, z) p lm ls = SplitLayout d z p lm ls [] []

half :: Rational
half = 0.5

splitRect :: (Direction2D, Dimension) -> Rectangle -> (Rectangle, Rectangle)
splitRect (R, n) r | n > 0 && n < t = (r{ rect_width  = t - n }, r{ rect_x = rect_x r + ii (t - n), rect_width  = n }) where t = rect_width r
splitRect (L, n) r | n > 0 && n < t = (r{ rect_width  = t - n,      rect_x = rect_x r + ii n },  r{ rect_width  = n }) where t = rect_width r
splitRect (D, n) r | n > 0 && n < t = (r{ rect_height = t - n }, r{ rect_y = rect_y r + ii (t - n), rect_height = n }) where t = rect_height r
splitRect (U, n) r | n > 0 && n < t = (r{ rect_height = t - n,      rect_y = rect_y r + ii n },  r{ rect_height = n }) where t = rect_height r
splitRect (R, _) r = splitVerticallyBy half r
splitRect (L, _) r = swap $ splitVerticallyBy half r
splitRect (D, _) r = splitHorizontallyBy half r
splitRect (U, _) r = swap $ splitHorizontallyBy half r

partitionStackM :: Monad m => (a -> m Bool) -> Maybe (Stack a) -> m (Maybe (Stack a), Maybe (Stack a))
partitionStackM _ Nothing = return (Nothing, Nothing)
partitionStackM p (Just (Stack f u d)) = do
  af <- p f
  (u1,u2) <- partitionM p u
  (d1,d2) <- partitionM p d
  return $ if af
    then (Just (Stack f u1 d1), refocus u2 d2)
    else (refocus u1 d1, Just (Stack f u2 d2))
  where
    refocus u (f:d) = Just $ Stack f u d
    refocus (f:u) d = Just $ Stack f u d
    refocus [] [] = Nothing

switchWindow :: SplitLayout lm ls Window -> Window -> Maybe (SplitLayout lm ls Window)
switchWindow l@(SplitLayout{ splitMains = m, splitSubs = s }) w
  | Just m' <- deleteOne w m = Just $ l{ splitMains = m', splitSubs = w:s }
  | Just s' <- deleteOne w s = Just $ l{ splitMains = w:m, splitSubs = s' }
  | otherwise = Nothing

instance (LayoutClass lm Window, LayoutClass ls Window) => LayoutClass (SplitLayout lm ls) Window where
  description l = description (splitMain l) ++ " split " ++ show (splitSize l) ++ show (splitDirection l) ++ " " ++ description (splitSub l)

  runLayout (Workspace wid l s) r = do
    (ss,sm) <- partitionStackM assignWindow s
    let
      (rm, rs) 
	| isNothing ss = (r, r)
	| otherwise = splitRect (splitDirection l, splitSize l) r
    (wrm, lm) <- runLayout (Workspace (wid ++ "M") (splitMain l) sm) rm
    (wrs, ls) <- runLayout (Workspace (wid ++ "S") (splitSub  l) ss) rs
    return (wrm ++ wrs, Just l
      { splitMain = fromMaybe (splitMain l) lm
      , splitSub  = fromMaybe (splitSub  l) ls
      , splitMains = maybe [] integrate sm
      , splitSubs = maybe [] integrate ss
      })
    where
    assignWindow w
      | w `elem` splitMains l = return False
      | w `elem` splitSubs l = return True
      | otherwise = hasProperty (splitProp l) w

  handleMessage l m
    | Just (SwitchWindow w) <- fromMessage m = return $ switchWindow l w
    | otherwise = fmap (\lm -> l{ splitMain = lm }) =.< handleMessage (splitMain l) m
