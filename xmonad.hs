{-# OPTIONS -Wall #-}
{-# LANGUAGE PatternGuards, FlexibleContexts #-}
import XMonad as X hiding (mouseResizeWindow)
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import XMonad.Actions.FlexibleResize
import XMonad.Layout.Column
import XMonad.Layout.Gaps
import qualified XMonad.Prompt as XP
import XMonad.Util.Run
import XMonad.Util.WindowProperties
import Control.Concurrent.MVar
import Control.Monad
import qualified Data.Map as Map
import System.Environment
import System.Exit
import Util
import Global
import Layout
import Pager
import Server
import Command
import Prompt

_browser :: String
_browser = "firefox"

isStuck :: Property
isStuck = Title "stuck term"

prompt :: XP.XPConfig
prompt = XP.defaultXPConfig
  { XP.bgColor = "#cccccc"
  , XP.fgColor = "#000000"
  , XP.bgHLight = "#000000"
  , XP.fgHLight = "#ffffcc"
  , XP.borderColor = "#0000cc"
  , XP.showCompletionOnTab = False
  }

layout = gaps [(U,topHeight)] $ splitLayout (R, 2+80*6) isStuck lmain lstuck
  where
  lmain = Full
  lstuck = Column 1

manager :: ManageHook
manager = composeAll
  [ isElem title ["Stripchart","xeyes","xload","xdaliclock","xrtail"] --> doIgnore
  , isElem className ["feh"] --> doFloat
  , propertyToQuery isStuck --> ask >>= doF . stickWindow
  ]

bind :: [((KeyMask, KeySym), X ())]
bind =
  [ ((wmod .|. shiftMask,   xK_Escape),	io (exitWith ExitSuccess))
  , ((wmod,		    xK_space),	refresh)
  , ((wmod .|. controlMask, xK_space),	restart "xmonad" True)

  , ((wmod,		    xK_c),	windows (W.swapUp . W.focusDown))
  , ((wmod .|. shiftMask,   xK_c),	promptRun prompt True)
  , ((wmod,		    xK_r),	windows (W.shiftMaster . W.focusUp))
  , ((wmod .|. shiftMask,   xK_r),	promptRun prompt False)
  , ((wmod,		    xK_l),	runTerm term)
  , ((wmod .|. shiftMask,   xK_l),	promptLogin prompt)

  , ((wmod,		    xK_i),	withFocused (\w -> io $ runProcessWithInput "xprop" ["-id",show w] "" >>= notify))
  , ((wmod,		    xK_d),	nextScreen)
  , ((wmod,		    xK_h),	prevWS)
  , ((wmod,		    xK_t),	windows W.focusUp)
  , ((wmod,		    xK_n),	windows W.focusDown)
  , ((wmod,		    xK_s),	nextWS)
  , ((wmod,		    xK_minus),	toggleWS)
  , ((wmod .|. shiftMask,   xK_minus),	withFocused (sendMessage . SwitchWindow))
  , ((wmod,		    xK_Return),	windows (W.view (head desktopIds)))

  , ((wmod,		    xK_semicolon), spawn ((if hostHome then "" else "xlock && ") ++ "sleep 2 && xset dpms force off"))
  , ((wmod .|. shiftMask,   xK_semicolon), promptOp prompt)
  , ((wmod,		    xK_q),	spawnp "xlock")
  , ((wmod,		    xK_x),	kill)
  , ((wmod,		    xK_b),	spawn "xbg && [ -p HOME/.xtail ] && touch HOME/.xtail")
  , ((wmod,		    xK_m),	withFocused float)
  , ((wmod .|. shiftMask,   xK_m),	withFocused (windows . W.sink))
  , ((wmod,		    xK_w),	windows W.shiftMaster)
  , ((wmod,		    xK_v),	windows W.swapDown)
  , ((wmod,		    xK_z),	withFocused hide)
  ]
  ++ zipWith (\i fk -> 
    ((0, fk),		windows (W.view i))) desktopIds [xK_F1..]
  ++ zipWith (\i fk -> 
    ((wmod, fk),	windows (W.shift i))) desktopIds [xK_F1..]
  ++ zipWith (\i fk -> 
    ((wmod, fk),	windows (W.view i))) desktopIds (xK_grave:[xK_1..])

mouse :: [((KeyMask, Button), Window -> X ())]
mouse = 
  [ ((wmod, button1),	    mouseMoveWindow)
  , ((wmod, button2),	    promptWindowOp prompt)
  , ((wmod, button3),	    mouseResizeWindow)
  ]

main :: IO ()
main = do
  args <- getArgs
  let new = "--resume" `notElem` args
  pagerLog <- pagerStart
  sct <- newEmptyMVar
  let startup = do
	_ <- io . tryPutMVar sct =<< getServerCommandType 
	when new $ mapM_ (spawnl . snd) startups
  xmonad $ defaultConfig
    { normalBorderColor = colorBG
    , focusedBorderColor = colorFG
    , X.terminal = Command.terminal term
    , layoutHook = layout
    , manageHook = manager
    , handleEventHook = \e -> io (readMVar sct) >>= \c -> serverEventHook c e
    , X.workspaces = desktopIds
    , modMask = wmod
    , keys = const $ Map.fromList bind
    , mouseBindings = const $ Map.fromList mouse
    , logHook = pagerLog
    , startupHook = startup
    , focusFollowsMouse = True
    }

{- TODO:
 -   menus
 -   main layouts
 -   floating/layering
 -   "icons"
 -   better resize
 -   mpc/dzen status
 -   transparent/root dzen
 -}
