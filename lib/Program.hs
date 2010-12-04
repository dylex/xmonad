{-# OPTIONS -Wall #-}
module Program
  ( Term(..), term, runTerm
  , notify
  , identWindow
  , runBrowser
  , runLogin
  , startups
  , programs
  , mixerSet
  ) where

import XMonad as X hiding (terminal)
import XMonad.Util.Run
import System.IO
import Util
import Param
import Ops

data Term = Term 
  { terminal :: String
  , termTitle :: Maybe String
  , termHold :: Bool
  , termRun :: Maybe Run
  }

term :: Term
term = Term
  { terminal = "mrxvt"
  , termTitle = Nothing
  , termHold = False
  , termRun = Nothing
  }

runTerm :: MonadIO m => Term -> m ()
runTerm t = spawnl $ terminal t 
  : maybe [] (\n -> ["-title",n]) (termTitle t)
  ++ (if termHold t then ["-hold","1"] else [])
  ++ maybe [] (("-e":) . unRun) (termRun t)

notify :: MonadIO m => String -> m ()
notify s = io $ do
  h <- spawnPipe "xmessage -file -"
  hPutStr h s
  hClose h

identWindow :: Window -> X ()
identWindow w = io $ runProcessWithInput "xprop" ["-id",show w] "" >>= notify

browser :: String
browser = "firefox"

runBrowser :: Maybe String -> X ()
runBrowser Nothing = spawnp browser
runBrowser (Just url) = spawnl [browser,url]

runLogin :: String -> X ()
runLogin h = runTerm $ term{ termTitle = Just h, termRun = Just (RunShell ("ssh " ++ h)) }

startups :: [(String, Run)]
startups = l pagerWidth where
  l = lif (isExec "xeyes")
      (geom topHeight $ \g -> ("xeyes", nice ["xeyes","-distance","-geometry",g]))
--    $ (if isExec "stripchart"
--	then geom 360 $ \g -> ("stripchart", nice ["-8","stripchart","--geometry",g])
--	else geom 120 $ \g -> ("xload", nice ["-8","xload","-bg","#3050A0","-fg","#F0E000","-nolabel","-update","30","-geometry",g]))
    $ lif (isExec "xdaliclock")
      (geom 220 $ \g -> ("xdaliclock", nice ["-5","xdaliclock","-transparent","-hex","-noseconds","-fg",colorRootFG,"-fn","-*-luxi sans-medium-r-*-*-*-400-*-*-*-*-iso8859-1","-geometry",g]))
    $ lif hostHome
      (push (80*5) $ \x -> ("xrtail", nice ["-5","xrtail","-geom","80x7" ++ x ++ "+0","-fn","5x8","-fg",colorRootFG,home++"/.xrw"]))
    $ const $
      -- FIXME -fn:
    [ ("stuck term", Run "xterm" ["-title","stuck term","-fn","-*-proggytiny-medium-*","-fb","-*-proggytiny-bold-*"{-,"-fn","6x10","-fb","6x10"-}])
    , ("xset", Run "xset" ["b","100","3520",show (if hostHome then 20 else 35 :: Int),"m","3","5","r","rate","250","30","s","0","+dpms","dpms","300","0","900"])
    ] 
--    ++ guard1 (isExec "xbg") ("xbg", Run "xbg" [])
  push w f r x = f ('+' : show x) : r x' where x' = x+w
  geom w f = push w (f . g) where
    g x = show w ++ "x" ++ show topHeight ++ x ++ "+0"
  lif False _ r x = r x
  lif True f r x = f r x
  nice = Run "nice" 

programs :: [(String, Run)]
programs = startups
  ++ guard1 hostHome ("vnc-greed", Run "vncviewer" ["localhost:28659"])
  ++ prog "gnumeric"
  ++ prog "gimp"
  ++ prog "xfig"
  ++ prog "xv"
  ++ prog "editres"
  ++ prog "xmag"
  ++ prog "xfontsel"
  ++ prog "firefox"
  ++ prog "chrome"
  ++ prog "nethack"
  ++ prog "xkill"
  where
  prog p = progArgs p []
  progArgs p a
    | isExec p = [(p, Run p a)]
    | otherwise = []

mixerSet :: MonadIO m => Ordering -> Int -> m ()
mixerSet d n 
  | osName == "Linux" = spawnl ["amixer","-q","-D","main","set","Master",show n ++ dirSign d]
  | osName == "FreeBSD" = spawnl ["/usr/sbin/mixer",if hostName == "druid" then "ogain" else "vol",dirSign d ++ show n]
  | otherwise = nop
  where
  dirSign LT = "-"
  dirSign EQ = "%"
  dirSign GT = "+"
