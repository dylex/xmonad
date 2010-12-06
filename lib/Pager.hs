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

data StackPos = Up | Focus | Down deriving (Eq, Ord)

data WinInfo = WinInfo
  { win :: Window
  , winName :: String
  , winPos :: !StackPos
  , winFloat :: !Bool
  }

getWinInfo :: WindowSet -> WindowSpace -> StackPos -> Window -> X WinInfo
getWinInfo set _ws p w = do
  n <- show =.< getName w
  return $ WinInfo
    { win = w
    , winName = n
    , winPos = p
    , winFloat = w `Map.member` W.floating set
    }

getWinList :: WindowSet -> WindowSpace -> X [WinInfo]
getWinList _ (W.Workspace{ W.stack = Nothing }) = return []
getWinList set ws@(W.Workspace{ W.stack = Just (W.Stack f u d) }) = mapM (uncurry $ getWinInfo set ws) $ (Focus,f) : map ((,) Up) u ++ map ((,) Down) d

winInfo :: WinInfo -> Int -> String
winInfo w l = dzenClickArea 3 ServerCommandWindowMenu [ii $ win w] $ take l $ winName w

deskInfo :: WindowSet -> Desktop -> WindowSpace -> X (String, [String])
deskInfo set d = wins >=. \(wm:wl) -> (line True wm, map (line False) wl) where
  line top w =
    (if cur then "pa"^/show (i*pagerDeskWidth) ++ "fg"^/"#404080" ++ "r"^/(show pagerDeskWidth ++ "x" ++ show fontSize) else "")
    ++ "pa"^/show (i*pagerDeskWidth)
    ++ (if winFloat w then "bg"^/fcol else "ib"^/"1")
    ++ "fg"^/(if cur then if winPos w == Down then "#C0C080" else "#FFFFC0"
		     else if winPos w == Down then "#8080C0" else "#C0C0FF")
    ++ dzenClickArea 1 cmd [arg]
      (winInfo w deskChars)
    ++ (if winFloat w then "bg"^/"" else "ib"^/"0")
    where
    (cmd, arg) = if not top || winPos w == Focus && cur then (ServerCommandFocus, ii $ win w) else (ServerCommandView, i) 
  fcol 
    | cur = "#408040"
    | otherwise = "#004000"
  wins ws@(W.Workspace{ W.stack = Just _ }) = getWinList set ws
  wins _ = asks theRoot >.= \r -> [WinInfo r tag Down False]
  cur = tag == W.currentTag set
  tag = show d
  i = fromEnum d

iconInfo :: WindowSet -> WindowSpace -> X String
iconInfo set ws = unwords . map icon =.< getWinList set ws where
  icon w =
    "fg"^/(if winPos w == Focus then colorFG else colorBG)
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
  io $ hPutStrLn h $ "^cs()\n^tw()" 
    ++ concat tl
    ++ '\n' : unlines (map concat (transpose bll))
    ++ "bg"^/"#400000" ++ il

pagerStart :: IO (X ())
pagerStart = pagerLog =.< runDzen dzenArgs
