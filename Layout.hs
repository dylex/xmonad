{-# OPTIONS -fno-warn-name-shadowing #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts, DeriveDataTypeable, PatternGuards #-}
module Layout
  ( SplitLayout
  , splitLayout
  , SplitMessage(..)
  ) where

import XMonad as X
import XMonad.StackSet as W
import XMonad.Util.Types
import XMonad.Util.WindowProperties
import Data.Maybe
import Util
import Param

-- based on ComboP
data SplitLayout lm ls a = SplitLayout 
  { splitDirection :: !Direction2D
  , splitSize :: !Dimension
  , splitProp :: !Property
  , splitMain :: !(lm a)
  , splitSub :: !(ls a)
  , splitMains, splitSubs :: ![a]
  , splitMainFocus, splitSubFocus :: !(Maybe a)
  } deriving (Show, Read)

data SplitMessage 
  = SwitchFocus -- switch focus to other side
  | SwitchWindow Window -- move window to other side
  | SplitModify (Stack Window -> Stack Window) -- apply a modification function to one side only
  | SplitModifies (Stack Window -> Stack Window) -- apply a modification function, possibly multiple times until focus returns to the same side
  deriving (Typeable)
instance Message SplitMessage

splitLayout :: (LayoutClass lm Window, LayoutClass ls Window) => (Direction2D, Dimension) -> Property -> lm Window -> ls Window -> SplitLayout lm ls Window
splitLayout (d, z) p lm ls = SplitLayout d z p lm ls [] [] Nothing Nothing

half :: Rational
half = 0.5

gaps :: Rectangle -> Rectangle
gaps (Rectangle x y w h) = Rectangle (x+1) (y+ii topHeight) (w-2) (h-ii topHeight-1)

shrinkBy :: Dimension -> Rectangle -> Rectangle
shrinkBy i (Rectangle x y w h) = Rectangle (x+ii i) (y+ii i) (w-2*i) (h-2*i)

splitRect :: (Direction2D, Dimension) -> Rectangle -> (Rectangle, Rectangle)
splitRect (R, n) (Rectangle x y w h) | n > 0 && n < w = (Rectangle x        y (w-n) h, Rectangle (x+ii (w-n)) y n h)
splitRect (L, n) (Rectangle x y w h) | n > 0 && n < w = (Rectangle (x+ii n) y (w-n) h, Rectangle x            y n h)
splitRect (D, n) (Rectangle x y w h) | n > 0 && n < h = (Rectangle x        y w (h-n), Rectangle x (y+ii (h-n)) w n)
splitRect (U, n) (Rectangle x y w h) | n > 0 && n < h = (Rectangle x (y+ii n) w (h-n), Rectangle x            y w n)
splitRect (R, _) r = splitVerticallyBy half r
splitRect (L, _) r = swap $ splitVerticallyBy half r
splitRect (D, _) r = splitHorizontallyBy half r
splitRect (U, _) r = swap $ splitHorizontallyBy half r

foldAt' :: Eq a => [a] -> a -> [a] -> Maybe ([a],[a])
foldAt' c x = fc c where
  fc _ [] = Nothing
  fc a (y:b) 
    | x == y = Just (a,b)
    | otherwise = fc (y:a) b

_foldAt :: Eq a => a -> [a] -> Maybe ([a],[a])
_foldAt = foldAt' []

refocusStack :: Eq a => a -> Stack a -> Stack a
refocusStack f' s@(Stack f u d)
  | f == f' = s
  | Just (d',u') <- foldAt' (f:d) f' u = Stack f' u' d'
  | Just (u',d') <- foldAt' (f:u) f' d = Stack f' u' d'
  | otherwise = s

partitionStackM :: (Monad m, Eq a) => (a -> m Bool) -> Maybe a -> Maybe a -> Maybe (Stack a) -> m (Maybe (Stack a), Maybe (Stack a))
partitionStackM _ _ _ Nothing = return (Nothing, Nothing)
partitionStackM p f1 f2 (Just (Stack f u d)) = do
  af <- p f
  (u1,u2) <- partitionM p u
  (d1,d2) <- partitionM p d
  return $ if af
    then (Just (Stack f u1 d1), refocus f2 u2 d2)
    else (refocus f1 u1 d1, Just (Stack f u2 d2))
  where
    refocus _ [] [] = Nothing
    refocus (Just f) u d
      | Just (d',u') <- foldAt' d f u = Just $ Stack f u' d'
      | Just (u',d') <- foldAt' u f d = Just $ Stack f u' d'
    refocus _ (f:u) d = Just $ Stack f u d
    refocus _ u (f:d) = Just $ Stack f u d

switchFocus :: Eq a => SplitLayout lm ls a -> Stack a -> Stack a
switchFocus SplitLayout{ splitMainFocus = Just mf, splitSubFocus = Just sf} s@(Stack f _ _)
  | sf == f = refocusStack mf s
  | otherwise = refocusStack sf s
switchFocus _ s = s

switchWindow :: SplitLayout lm ls Window -> Window -> Maybe (SplitLayout lm ls Window)
switchWindow l@(SplitLayout{ splitMains = m, splitSubs = s }) w
  | Just m' <- deleteOne w m = Just $ l{ splitMains = m', splitSubs = w:s }
  | Just s' <- deleteOne w s = Just $ l{ splitMains = w:m, splitSubs = s' }
  | otherwise = Nothing

splitModify :: Eq a => SplitLayout lm ls a -> (Stack a -> Stack a) -> (Stack a -> Stack a)
splitModify l m (Stack f u d) = Stack f' (u2++u') (d'++d2) where
  Stack f' u' d' = m $ Stack f u1 d1
  (u1,u2) = pf u
  (d1,d2) = pf d
  pf
    | f `elem` splitMains l = partitionElems (splitMains l)
    | f `elem` splitSubs l = partitionElems (splitSubs l)
    | otherwise = swap . partitionElems (splitMains l ++ splitSubs l)

splitModifies :: Eq a => SplitLayout lm ls a -> (Stack a -> Stack a) -> (Stack a -> Stack a)
splitModifies l m s@(Stack f _ _) = until (tf . W.focus) m (m s) where
  tf
    | f `elem` splitMains l = (`elem` splitMains l)
    | f `elem` splitSubs l = (`elem` splitSubs l)
    | otherwise = (`notElem` splitMains l ++ splitSubs l)

instance (LayoutClass lm Window, LayoutClass ls Window) => LayoutClass (SplitLayout lm ls) Window where
  description l = description (splitMain l) ++ " split " ++ show (splitSize l) ++ show (splitDirection l) ++ " " ++ description (splitSub l)

  runLayout (Workspace wid l s) r = do
    (ss,sm) <- partitionStackM assignWindow (splitSubFocus l) (splitMainFocus l) s
    let
      (rm, rs) 
        | isNothing ss = (r', r')
        | otherwise = splitRect (splitDirection l, splitSize l) r'
    (wrm, lm) <- runLayout (Workspace (wid ++ "M") (splitMain l) sm) rm
    (wrs, ls) <- runLayout (Workspace (wid ++ "S") (splitSub  l) ss) rs
    return (map (second $ shrinkBy 1) $ wrm ++ wrs, Just l
      { splitMain = fromMaybe (splitMain l) lm
      , splitSub  = fromMaybe (splitSub  l) ls
      , splitMains = maybe [] integrate sm
      , splitSubs  = maybe [] integrate ss
      , splitMainFocus = fmap W.focus sm
      , splitSubFocus = fmap W.focus ss
      })
    where
    r' = gaps r
    assignWindow w
      | w `elem` splitMains l = return False
      | w `elem` splitSubs  l = return True
      | otherwise = hasProperty (splitProp l) w

  handleMessage l m = case fromMessage m of
    Just SwitchFocus -> windows (modify' (switchFocus l)) >. Nothing
    Just (SwitchWindow w) -> return $ switchWindow l w
    Just (SplitModify f) -> windows (modify' (splitModify l f)) >. Nothing
    Just (SplitModifies f) -> windows (modify' (splitModifies l f)) >. Nothing
    Nothing -> fmap (\lm -> l{ splitMain = lm }) =.< handleMessage (splitMain l) m -- fallback to sub?
