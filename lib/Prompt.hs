{-# OPTIONS -Wall #-}
{-# LANGUAGE PatternGuards, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification, StandaloneDeriving #-}
module Prompt
  ( promptRun
  , promptLogin
  , promptOp, promptWindowOp
  ) where

import XMonad
import XMonad.Prompt
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.CopyWindow as XCW
import XMonad.Util.NamedWindows
import Control.Monad
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import System.Directory
import System.FilePath
import Util
import Param
import Ops
import Program
import Completer

readFile' :: String -> IO String
readFile' f = readFile f `catch` \_ -> return ""

readDir' :: Bool -> String -> IO [String]
readDir' dots "" = readDir' dots "."
readDir' False d = filter (('.' /=) . head) =.< readDir' True d
readDir' True d = getDirectoryContents d `catch` \_ -> return []

breakLast :: (a -> Bool) -> [a] -> ([a], [a])
breakLast f s = fromMaybe ([],s) $ bl s where
  bl [] = Nothing
  bl (x:l)
    | f x = r `coalesce` Just ([],l)
    | otherwise = r
    where r = fmap (first (x:)) $ bl l

class Read a => Input a where
  input :: String -> Maybe a
  input = readMaybe
instance Input String where input = Just

data Closure r = forall a . Output a => Closure
  { _ccFun :: a -> r
  , _ccArg :: a
  }
instance Show (Closure r) where showsPrec d (Closure _ a) = showParen (d > 10) $ showString "Closure " . showsPrec 11 a
instance Output (Closure r) where output (Closure _ a) = output a

runClosure :: Closure r -> r
runClosure (Closure f a) = f a

completeClosure :: Output a => Completer a -> (a -> r) -> Completer (Closure r)
completeClosure c f = fmap (Closure f) c


data Prompt = Prompt String
instance XPrompt Prompt where
  showXPrompt (Prompt p) = p ++ ": "
  commandToComplete _ c = c
  nextCompletion _ = getNextCompletion

xpConfig :: XPConfig
xpConfig = defaultXPConfig
  { bgColor = "#cccccc"
  , fgColor = "#000000"
  , bgHLight = "#000000"
  , fgHLight = "#ffffcc"
  , borderColor = "#0000cc"
  , showCompletionOnTab = False
  }

unknown :: Output a => a -> X ()
unknown _ = withDisplay (io . (`bell` 100))

tryMaybe :: Output e => e -> (a -> X ()) -> Maybe a -> X ()
tryMaybe = maybe . unknown

_tryInput :: Input a => (a -> X ()) -> String -> X ()
_tryInput f i = tryMaybe i f $ input i

tryAccept :: Output a => (a -> X ()) -> Completer a -> String -> X ()
tryAccept f c s = tryMaybe s f =<< io (accept c s)

prompt :: Output a => String -> Completer a -> (a -> X ()) -> X ()
prompt p c f = mkXPrompt (Prompt p) xpConfig (complFunction c) $ tryAccept f c


filePath :: CS
filePath = unlessNull $ do
  d <- pureCompleter1 splitFileName
  f <- get
  let dot = case f of { '.':_ -> True ; _ -> False }
  l <- io $ readDir' dot d
  fmap (d++) $ oneOf l

type ShellCommand = (Words String String)
shellCommand :: IO (Completer ShellCommand)
shellCommand = do
  path <- getSearchPath
  cmds <- concatMapM (readDir' False) path
  let cmd = suggest $ oneOfSet (Set.fromList cmds) `mappend` filePath
      arg = anything -- TODO
  return $ wordPair (word1 cmd) arg

promptRun :: Bool -> X ()
promptRun t = do
  cmd <- io shellCommand
  if t 
    then prompt "run" cmd runInTerm
    else prompt "run" (oneOf (map fst programs) `eitherOr` cmd) runProg

shellCommandRun :: ShellCommand -> Run
shellCommandRun = RunShell . output

runInTerm :: ShellCommand -> X ()
runInTerm c = runTerm $ term{ termTitle = Just (output c), termHold = True, termRun = Just (shellCommandRun c) }

runProg :: Either String ShellCommand -> X ()
runProg (Left p) = run $ fromJust $ lookup p programs
runProg (Right c) = run (shellCommandRun c)


loginHost :: IO CS
loginHost = suggest . oneOf =.< mapMaybe hostLine . lines =.< readFile' (home ++ "/.ssh/config") where
  hostLine s 
    | ["Host",h] <- words s
    , '*' `notElem` h && '?' `notElem` h = Just h
    | otherwise = Nothing

promptLogin :: X ()
promptLogin = do
  c <- io $ loginHost
  prompt "ssh" c runLogin


instance Output Desktop

desktop :: Completer Desktop
desktop = oneOf desktops

instance Output NamedWindow where
  output w = show w ++ '#' : show (unName w)

instance Input (Window, String) where
  input s = fmap (\w -> (w, n)) $ readMaybe i where
    (n,i) = breakLast ('#'==) s
instance Input Window where
  input s = fmap fst (input s :: Maybe (Window, String))

window :: X (Completer NamedWindow)
window = do
  ws <- gets windowset
  wn <- mapM getName $ W.allWindows ws
  return $ oneOf wn

data Op = Op
  { _opName :: String
  , _opFunction :: X ()
  }

instance Eq Op where Op n1 _ == Op n2 _ = n1 == n2
instance Show Op where showsPrec d (Op n _) = showParen (d > 10) $ showString "Op " . showString n
instance Output Op where output (Op n _) = output n

type OpClosure = Closure (X ())

ops :: Maybe NamedWindow -> X (Completer (Words Op OpClosure))
ops defw = do
  win <- maybe (fmap Just =.< window) (const $ return $ return Nothing) defw
  defd <- W.currentTag =.< gets windowset
  let
    op n c f = (Op n (f Nothing), completeClosure c (f . Just))
    o_ n (f :: X ()) = 
      op n nop (const f)
    ow n (f :: Window -> X ()) = 
      op n win (xw f)
    od n (f :: WorkspaceId -> X ()) =
      op n desktop (xd f)
    odw n (f :: WorkspaceId -> Window -> X ()) = 
      op n (word1 desktop `wordPair` win) (xdw f)

    xw f w = maybe (withFocused f) f $ fmap unName $ join w `coalesce` defw
    xd f = f . fromMaybe defd . fmap show
    xdw f dw = xw (xd f $ fmap headWord dw) $ tailWords =<< dw
  return $ switchCmd $
    [o_ "refresh"	refresh
    ,o_ "rescreen"	rescreen
    ,od "view"		$ windows . W.view
    ,ow "manage"	manage
    ,ow "unmanage"	unmanage
    ,ow "kill"		killWindow
    ,ow "hide"		hide
    ,ow "icon"		$ windows . W.shiftWin iconWorkspace
    ,ow "reveal"	reveal
    ,ow "focus"		$ windows . W.focusWindow
    ,ow "xfocus"	setFocusX
    ,ow "float"		float
    ,ow "sink"		$ windows . W.sink
    ,ow "switch"	switchWindow
    ,odw "shift"	$ \i -> windows . W.shiftWin i
    ,ow "stick"		$ windows . stickWindow
    ,odw "copy"		$ \i w -> windows $ XCW.copyWindow w i
    ,ow "remove"	$ windows . W.modify Nothing . W.filter . (/=)
    ,ow "ident"		identWindow
    ]

promptOp :: X ()
promptOp = do
  o <- ops Nothing
  prompt "" o runOp

promptWindowOp :: Window -> X ()
promptWindowOp w = do
  nw <- getName w
  o <- ops (Just nw)
  prompt (show nw) o runOp

runOp :: (Words Op OpClosure) -> X ()
runOp (Words (Op _ f) Nothing) = f
runOp (Words _ (Just cc)) = runClosure cc
