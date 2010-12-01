{-# OPTIONS -Wall #-}
module Pager 
  ( pagerWidth
  , pagerStart
  ) where

import XMonad as X
import qualified XMonad.StackSet as W
import XMonad.Util.NamedWindows
import Control.Monad
import Data.List
import qualified Data.Map as Map
import System.IO
import Util
import Param
import Server
import Dzen

deskWidth :: Int
deskWidth = 75

pagerWidth :: Int
pagerWidth = deskWidth*length desktops

fontSize :: Int
fontSize = 8

deskChars :: Int
deskChars = deskWidth `div` 5

dzenArgs :: [String]
dzenArgs = dzenDefaultArgs ++
  ["-fn","-*-fixed-*-*-*-*-" ++ show fontSize ++ "-*-*-*-*-*-*-*"
  ,"-l",show (pred $ topHeight `div` fontSize)
  ,"-h",show fontSize
  ,"-w",show pagerWidth
  ,"-e","entertitle=uncollapse;leaveslave=collapse;button2=togglestick;button4=scrollup;button5=scrolldown"
  ]

data WinInfo = WinInfo
  { _win :: Window
  , _winName :: String
  , _winMaster :: !Bool
  , _winFocus :: !Bool
  , _winFloating :: !Bool
  }

deskInfo :: WindowSet -> Desktop -> Maybe WindowSpace -> X (String, [String])
deskInfo set d = wins >=. \(wm:wl) -> (line True wm, map (line False) wl) where
  line top (WinInfo w name master fcs flt) =
    "pa"^/show (i*deskWidth)
    ++ "bg"^/(if flt then "#408040" else "")
    ++ "fg"^/(if fcs then if current then "#FFFFBB" else "#BBBBFF"
		       else if current && top then colorFG else colorBG)
    ++ dzenClickArea 1 cmd [arg]
      (take deskChars name)
    where
    (cmd, arg) = if not top || master && current then (ServerCommandFocus, ii w) else (ServerCommandView, i) 
  wins (Just (W.Workspace{ W.stack = Just stack })) =
    zipWithM wininfo (W.integrate stack) (True:repeat False) where
    wininfo w m = do
      n <- show =.< getName w
      return $ WinInfo w n m (w == W.focus stack) (w `Map.member` W.floating set)
  wins _ = return [WinInfo undefined tag False False False]
  current = tag == W.currentTag set
  tag = show d
  i = fromEnum d

pagerLog :: Handle -> X ()
pagerLog h = do
  s <- gets windowset
  let ws = W.workspaces s
  (tl,bll) <- mapAndUnzipM (\i -> deskInfo s i $ find ((show i ==) . W.tag) ws) desktops
  io $ hPutStrLn h $ "^cs()\n^tw()" 
    ++ concat tl
    ++ '\n' : unlines (map concat (transpose bll))

pagerStart :: IO (X ())
pagerStart = pagerLog =.< runDzen dzenArgs
