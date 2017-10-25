module Pager 
  ( pagerStart
  ) where

import           Control.Monad (mapAndUnzipM)
import           Data.List (find, transpose)
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           System.IO (Handle, hPutStrLn, hFlush)
import qualified XMonad as X
import qualified XMonad.StackSet as W
import           XMonad.Util.NamedWindows (getName)

import           Util
import           Param
import           Server
import           Dzen

fontSize :: Int
fontSize = 8

deskChars :: Int
deskChars = pagerDeskWidth `div` 5

dzenArgs :: [String]
dzenArgs = dzenDefaultArgs ++
  ["-fn","-*-fixed-*-*-*-*-" ++ show fontSize ++ "-*-*-*-*-*-iso10646-1"
  ,"-l",show (pred $ topHeight `div` fontSize)
  ,"-h",show fontSize
  ,"-w",show pagerWidth
  ,"-e","entertitle=uncollapse;leaveslave=collapse;button2=togglestick;button4=scrollup;button5=scrolldown"
  ]

data StackPos = Up | Focus | Down deriving (Eq, Ord)

data WinInfo = WinInfo
  { win :: X.Window
  , winName :: String
  , winPos :: !StackPos
  , winFloat :: !Bool
  }

getWinInfo :: X.WindowSet -> X.WindowSpace -> StackPos -> X.Window -> X.X WinInfo
getWinInfo set _ws p w = do
  n <- show =.< getName w
  return WinInfo
    { win = w
    , winName = n
    , winPos = p
    , winFloat = w `Map.member` W.floating set
    }

getWinList :: X.WindowSet -> X.WindowSpace -> X.X [WinInfo]
getWinList _ (W.Workspace{ W.stack = Nothing }) = return []
getWinList set ws@(W.Workspace{ W.stack = Just (W.Stack f u d) }) = mapM (uncurry $ getWinInfo set ws) $ (Focus,f) : map ((,) Up) u ++ map ((,) Down) d

winInfo :: WinInfo -> Int -> String
winInfo w l = dzenClickArea 3 ServerCommandWindowMenu [ii $ win w] $ take l $ winName w

deskInfo :: X.WindowSet -> Desktop -> X.WindowSpace -> X.X (String, [String])
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
  wins _ = X.asks X.theRoot >.= \r -> [WinInfo r tag Down False]
  cur = tag == W.currentTag set
  tag = show d
  i = fromEnum d

iconInfo :: X.WindowSet -> X.WindowSpace -> X.X String
iconInfo set ws = unwords . map icon =.< getWinList set ws where
  icon w =
    "fg"^/(if winPos w == Focus then "#FFFFC0" else "#C0C0F0")
    ++ "ib"^/"0"
    ++ dzenClickArea 1 ServerCommandShift [ii $ win w, -1]
      (winInfo w 60)
    ++ "ib"^/"1"

pagerLog :: Handle -> X.X ()
pagerLog h = do
  s <- X.gets X.windowset
  let ws = W.workspaces s
      fd i = fromJust $ find ((i ==) . W.tag) ws
  (tl,bll) <- mapAndUnzipM (\i -> deskInfo s i $ fd $ show i) desktops
  il <- iconInfo s (fd $ show iconDesktop)
  X.io $ do
  hPutStrLn h $ "^cs()\n^tw()" 
    ++ concat tl
    ++ '\n' : unlines (map concat (transpose bll))
    ++ "bg"^/"#400000" ++ il
  hFlush h

pagerStart :: IO (X.X ())
pagerStart = pagerLog =.< runDzen dzenArgs
