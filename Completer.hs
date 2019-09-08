{-# LANGUAGE PatternGuards, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
module Completer
  ( Output(..), outputAs
  , Completer, CS
  , complFunction
  , accept

  , anything
  , unlessNull
  , suggest
  , eitherOr
  , oneOf, oneOfSet
  , word, word1
  , Words(..), wordPair
  , switchCmd, switchCmdMap
  , pureCompleter1
  ) where

import Prelude hiding (Word)

import           Control.Applicative (Applicative(..))
import           Control.Monad ((>=>), liftM, liftM2)
import           Data.Char (isSpace)
import           Data.List (nub, find)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe, maybeToList)
import qualified Data.Set as Set

import XMonad
import XMonad.Prompt
import Util

listToMaybe' :: [x] -> Maybe x
listToMaybe' [x] = Just x
listToMaybe' _ = Nothing

match :: Eq a => [a] -> [a] -> Maybe [a]
match [] _ = Just []
match s [] = Just s
match (x:s) (y:t)
  | x == y = match s t
  | otherwise = Nothing

splitWith :: (a -> Bool) -> [a] -> Maybe ([a], [a])
splitWith f s = guard1 (not (null r)) (w, r') where
  s' = dropWhile f s
  (w,r) = break f s'
  r' = dropWhile f r

splitWord :: String -> Maybe (String, String)
splitWord = splitWith isSpace

type Completion a = (a, String)

entire :: a -> Completion a
entire = (, "")

entires :: [Completion a] -> [a]
entires = mapMaybe f where
  f (x, "") = Just x
  f _ = Nothing

class Show a => Output a where
  output :: a -> String
  output = show

instance Output String where output = id

instance Output () where output _ = ""
instance Output [String] where output = concatMap output
instance Output a => Output (Maybe a) where output = maybe "" output
instance (Output a, Output b) => Output (Either a b) where output = either output output
instance (Output a, Output b) => Output (a, b) where output (a, b) = output a ++ output b

data OutputAs a = OutputAs String !a
instance Show (OutputAs a) where showsPrec d (OutputAs s _) = showParen (d > 10) $ showString "OutputAs " . showsPrec 11 s
instance Output (OutputAs a) where output (OutputAs s _) = s

outputAs :: Output s => s -> a -> OutputAs a
outputAs = OutputAs . output

matches :: Output a => [a] -> String -> [Completion a]
matches l s = mapMaybe (\x -> fmap ((,) x) $ match s $ output x) l

-- A Completer may return both reductions (prefix matches of input) and completions (matches with input as prefix), but is otherwise quite similar to ReadS
newtype Completer a = Completer { runCompleter :: String -> IO [Completion a] }
type CS = Completer String

complete :: Completer a -> String -> IO [a]
complete c = entires .=< runCompleter c

-- completions that are valid assuming the input is complete
completed :: Output a => Completer a -> String -> IO [a]
completed c s = do
  l <- complete c s
  return $ maybeToList (listToMaybe' l `coalesce` find ((s ==) . output) l) `coalesce` l

-- the single valid completion, if there is one
accept :: Output a => Completer a -> String -> IO (Maybe a)
accept c s = do
  l <- complete c s
  return $ listToMaybe' l `coalesce` find ((s ==) . output) l

complFunction :: Output a => Completer a -> ComplFunction
complFunction c s = tail . nub . (s:) . map output =.< complete c s

liftC :: ([Completion a] -> [Completion b]) -> Completer a -> Completer b
liftC f c = Completer $ liftM f . runCompleter c

liftC2 :: ([Completion a] -> [Completion b] -> [Completion c]) -> Completer a -> Completer b -> Completer c
liftC2 f c1 c2 = Completer $ \s -> liftM2 f (runCompleter c1 s) (runCompleter c2 s)

pureCompleter :: (String -> [Completion a]) -> Completer a
pureCompleter f = Completer $ return . f

pureCompleter1 :: (String -> Completion a) -> Completer a
pureCompleter1 f = pureCompleter $ return . f

instance Semigroup (Completer a) where
  (<>) = liftC2 (<>)

instance Monoid (Completer a) where
  mempty = pureCompleter $ const []
  mappend = liftC2 mappend

instance Nullable (Completer a) where
  isNull = error "isNull Completer"
  nnull = mempty
  coalesce = liftC2 coalesce

instance Functor Completer where 
  fmap = liftC . map . first

instance Applicative Completer where
  pure = pureCompleter1 . (,)
  f <*> a = f >>= (`fmap` a)

instance Monad Completer where
  return = pure
  fail = Completer . const . fail -- Completer $ const $ return []
  m >>= f = Completer $ runCompleter m >=> concatMapM (uncurry $ runCompleter . f)

instance MonadState String Completer where
  get = Completer $ \s -> return [(s,s)]
  put s = Completer $ \_ -> return [((),s)]

instance MonadIO Completer where
  liftIO f = Completer $ \s -> f >.= return . (, s)

whenNull :: [a] -> Completer a -> Completer a
whenNull d c = Completer $ \s ->
  if null s
    then return $ map entire d
    else runCompleter c s

nonNull :: Completer a -> Completer a
nonNull = whenNull []

unlessNull :: Completer [a] -> Completer [a]
unlessNull = whenNull [[]]

anything :: CS
anything = pureCompleter1 entire

something :: CS
something = nonNull anything

eitherOr :: Completer a -> Completer b -> Completer (Either a b)
eitherOr = liftC2 $ \l r -> map (first Left) l ++ map (first Right) r

cons :: Completer a -> Completer [a] -> Completer [a]
cons = liftM2 (:)

_star :: Completer a -> Completer [a]
_star c = unlessNull $ cons c (_star c)

suggest :: CS -> CS
suggest = (`mappend` something)

oneOf :: Output a => [a] -> Completer a
oneOf = pureCompleter . matches

-- only does completions
oneOfSet :: Set.Set String -> CS
oneOfSet l = pureCompleter $ \s -> let
  (_, e, b) = Set.splitMember s l
  (a, _) = Set.split (init s ++ [succ $ last s]) b
  in map entire $ (if e then (s:) else id) $ Set.elems $ if null s then l else a

_stringify :: Output a => Completer a -> CS
_stringify = fmap output

data Word a 
  = Frag { unWord :: !a }
  | Word { unWord :: !a } 
  deriving (Show)
instance Output a => Output (Word a) where 
  output (Frag a) = output a
  output (Word a) = output a ++: ' '

data Words a b = Words { headWord :: !a, tailWords :: Maybe b } deriving (Show)
instance (Output a, Output b) => Output (Words a b) where
  output (Words a Nothing) = output a
  output (Words a (Just b)) = output a ++ ' ' : output b

-- works for completions and reductions, but only with on spaces
word1 :: Output a => Completer a -> Completer (Word a)
word1 c = Completer $ \s -> maybe (runCompleter (fmap Frag c) s) (\(w,r) -> map ((, r) . Word) =.< completed c w) $ splitWord s

-- only works for reductions, but with multiple words
word :: Completer a -> Completer (Word a)
word = liftC $ mapMaybe reWord where
  reWord (x,c:r) | isSpace c = Just (Word x,r)
  reWord (x,"") = Just (Frag x,"")
  reWord _ = Nothing

wordPair :: Completer (Word a) -> Completer b -> Completer (Words a b)
wordPair cw cr = cw >>= wf where
  wf (Frag w) = return (Words w Nothing)
  wf (Word w) = fmap (Words w . Just) cr

switchWord :: Completer (Word a) -> (a -> Maybe (Completer b)) -> Completer (Words a b)
switchWord cc af = cc >>= cf where
  cf (Word w) | Just cr <- af w = fmap (Words w . Just) cr
  cf w = return $ Words (unWord w) Nothing

switchCmd :: (Eq a, Output a) => [(a, Completer b)] -> Completer (Words a b)
switchCmd cm = switchWord (word $ oneOf $ map fst cm) (`lookup` cm)

switchCmdMap :: Map.Map String (Completer a) -> Completer (Words String a)
switchCmdMap cm = switchWord (word1 $ oneOfSet $ Map.keysSet cm) (`Map.lookup` cm)

