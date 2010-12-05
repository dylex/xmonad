{-# OPTIONS -Wall #-}
module Pager 
  ( pagerStart
  ) where

import XMonad as X
import qualified XMonad.StackSet as W
import XMonad.Util.NamedWindows
import Control.Monad
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.IO
import Util
import Param
import Server
import Dzen

fontSize :: Int
fontSize = 8

deskChars :: Int
deskChars = pagerDeskWidth `div` 5

dzenArgs :: [String]
dzenArgs = dzenDefaultArgs ++
  ["-fn","-*-fixed-*-*-*-*-" ++ show fontSize ++ "-*-*-*-*-*-*-*"
  ,"-l",show (pred $ topHeight `div` fontSize)
  ,"-h",show fontSize
  ,"-w",show pagerWidth
  ,"-e","entertitle=uncollapse;leaveslave=collapse;button2=togglestick;button4=scrollup;button5=scrolldown"
  ]

data WinInfo = WinInfo
  { win :: Window
  , winName :: String
  , winMaster :: !Bool
  , winFocus :: !Bool
  , winFloating :: !Bool
  }

getWinInfo :: WindowSet -> WindowSpace -> Window -> X WinInfo
getWinInfo set ws w = do
  n <- show =.< getName w
  return $ WinInfo
    { win = w
    , winName = n
    , winMaster = iss (head . W.integrate)
    , winFocus = iss W.focus
    , winFloating = w `Map.member` W.floating set
    }
  where
    iss f = maybe False ((w ==) . f) $ W.stack ws

winInfo :: WinInfo -> Int -> String
winInfo w l = dzenClickArea 3 ServerCommandWindowMenu [ii $ win w] $ take l $ winName w

deskInfo :: WindowSet -> Desktop -> WindowSpace -> X (String, [String])
deskInfo set d = wins >=. \(wm:wl) -> (line True wm, map (line False) wl) where
  line top wi@(WinInfo w _ master fcs flt) =
    (if cur then "pa"^/show (i*pagerDeskWidth) ++ "fg"^/"#404080" ++ "r"^/(show pagerDeskWidth ++ "x" ++ show fontSize) else "")
    ++ "pa"^/show (i*pagerDeskWidth)
    ++ (if flt then "bg"^/"#408040" ++ "ib"^/"0" else "")
    ++ "fg"^/(if cur then if fcs then "#FFFFBB" else colorFG 
		     else if fcs then "#BBBBFF" else colorBG)
    ++ dzenClickArea 1 cmd [arg]
      (winInfo wi deskChars)
    ++ (if flt then "bg"^/"" ++ "ib"^/"1" else "")
    where
    (cmd, arg) = if not top || master && cur then (ServerCommandFocus, ii w) else (ServerCommandView, i) 
  wins ws@(W.Workspace{ W.stack = Just stack }) =
    mapM (getWinInfo set ws) (W.integrate stack)
  wins _ = asks theRoot >.= \r -> [WinInfo r tag False False False]
  cur = tag == W.currentTag set
  tag = show d
  i = fromEnum d

iconInfo :: WindowSet -> WindowSpace -> X String
iconInfo set ws = unwords . map icon =.< mapM (getWinInfo set ws) (W.integrate' (W.stack ws)) where
  icon w =
    "fg"^/(if winFocus w then colorFG else colorBG)
    ++ "ib"^/"0"
    ++ dzenClickArea 1 ServerCommandShift [ii $ win w, -1]
      (winInfo w 60)
    ++ "ib"^/"1"

pagerLog :: Handle -> X ()
pagerLog h = do
  s <- gets windowset
  let ws = W.workspaces s
      fd i = fromJust $ find ((i ==) . W.tag) ws
  (tl,bll) <- mapAndUnzipM (\i -> deskInfo s i $ fd $ show i) desktops
  il <- iconInfo s (fd $ show iconDesktop)
  io $ hPutStrLn h $ "^cs()\n^tw()^ib(1)" 
    ++ concat tl
    ++ "\n^ib(1)" ++ unlines (map concat (transpose bll))
    ++ "bg"^/"#400000" ++ il

pagerStart :: IO (X ())
pagerStart = pagerLog =.< runDzen dzenArgs
