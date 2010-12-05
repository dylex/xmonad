{-# OPTIONS -Wall #-}
{-# LANGUAGE PatternGuards, FlexibleContexts #-}
import XMonad as X hiding (mouseResizeWindow)
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.CopyWindow as XCW
import XMonad.Actions.CycleWS
import XMonad.Actions.FlexibleResize
import XMonad.Layout.Column
import XMonad.Layout.Gaps
import XMonad.Util.Run
import XMonad.Util.WindowProperties
import Control.Concurrent.MVar
import Control.Monad
import qualified Data.Map as Map
import Data.Ratio ((%))
import System.Environment
import System.Exit
import Graphics.X11.ExtraTypes.XF86
import Util
import Param
import Ops
import Layout
import Pager
import Server
import Program
import Prompt
--import Selection

isStuck :: Property
isStuck = Title "stuck term"

layoutGap = gaps [(U,topHeight)]

layout = layoutGap $ splitLayout (R, 2+80*6) isStuck lmain lstuck
  where
  lmain = Full
  lstuck = Column 1

iconLayout = layoutGap $ Tall 1 0 (1%2)

manager :: ManageHook
manager = composeAll
  [ isElem title ["Stripchart","xeyes","xload","xdaliclock","xrtail"] --> doIgnore
  , isElem className ["feh"] <||> isElem title ["Event Tester"] --> doFloat
  , propertyToQuery isStuck --> ask >>= doF . stickWindow
  ]

bind :: [((KeyMask, KeySym), X ())]
bind =
  [ ((wmod,		    xK_Escape),	XCW.kill1)
  , ((wmod .|. shiftMask,   xK_Escape),	io (exitWith ExitSuccess))

  -- xK_apostrophe
  -- xK_comma
  -- xK_period
  -- xK_p
  --, ((wmod,		    xK_p),	withSelection trace)
  -- xK_y
  , ((wmod,		    xK_f),	runBrowser Nothing)
  --, ((wmod,		    xK_g),	runTerm term{ termRun = Just (Run "elinks") })
  , ((wmod,		    xK_c),	windows (W.swapUp . W.focusDown))
  , ((wmod .|. shiftMask,   xK_c),	promptRun True)
  , ((wmod,		    xK_r),	windows (W.shiftMaster . W.focusUp))
  , ((wmod .|. shiftMask,   xK_r),	promptRun False)
  , ((wmod,		    xK_l),	runTerm term)
  , ((wmod .|. shiftMask,   xK_l),	promptLogin)
  -- xK_slash
  -- xK_equal
  -- xK_backslash

  -- xK_a
  -- xK_o
  -- xK_e
  -- xK_u
  , ((wmod,		    xK_i),	withFocused (\w -> io $ runProcessWithInput "xprop" ["-id",show w] "" >>= notify))
  --, ((wmod .|. shiftMask,   xK_i),	runLogin "icicle")
  , ((wmod,		    xK_d),	nextScreen)
  , ((wmod .|. shiftMask,   xK_d),	runLogin "dylex")
  , ((wmod,		    xK_h),	windows $ viewDesk predWrap)
  , ((wmod .|. shiftMask,   xK_h),	windows $ shiftDesk predWrap)
  , ((wmod,		    xK_t),	windows W.focusUp)
  , ((wmod,		    xK_n),	windows W.focusDown)
  , ((wmod,		    xK_s),	windows $ viewDesk succWrap)
  , ((wmod .|. shiftMask,   xK_s),	windows $ shiftDesk predWrap)
  , ((wmod,		    xK_minus),	toggleWS)
  , ((wmod .|. shiftMask,   xK_minus),	withFocused (sendMessage . SwitchWindow))
  , ((wmod,		    xK_Return),	windows $ W.view $ show $ head desktops)

  , ((wmod,		    xK_semicolon), spawn ((if hostHome then "" else "xlock && ") ++ "sleep 2 && xset dpms force off"))
  , ((wmod .|. shiftMask,   xK_semicolon), promptOp)
  , ((wmod,		    xK_q),	spawnp "xlock")
  -- j
  -- k
  , ((wmod,		    xK_x),	kill)
  , ((wmod,		    xK_b),	spawn "xbg && [ -p HOME/.xtail ] && touch HOME/.xtail")
  , ((wmod,		    xK_m),	withFocused float)
  , ((wmod .|. shiftMask,   xK_m),	withFocused (windows . W.sink))
  , ((wmod,		    xK_w),	windows W.shiftMaster)
  , ((wmod,		    xK_v),	windows W.swapDown)
  , ((wmod,		    xK_z),	windows $ W.shift $ show iconDesktop)

  , ((wmod,		    xK_space),	refresh)
  , ((wmod .|. controlMask, xK_space),	restart "xmonad" True)

  , ((0,         xF86XK_AudioLowerVolume),  mixerSet LT 1)
  , ((0,         xF86XK_AudioRaiseVolume),  mixerSet GT 1)
  , ((wmod .|. shiftMask, xK_KP_Add),	    mixerSet LT 1)
  , ((wmod .|. shiftMask, xK_KP_Subtract),  mixerSet GT 1)
  , ((wmod,               xK_KP_Enter),	    mixerSet EQ 12)
  , ((wmod .|. shiftMask, xK_KP_Enter),	    mixerSet EQ 75)
  , ((wmod .|. shiftMask, xK_KP_Insert),  mpc "-p")
  , ((mod5Mask,           xK_KP_Insert),  mpc "-p")
  , ((wmod .|. shiftMask, xK_KP_Delete),  mpc "-r")
  , ((mod5Mask,           xK_KP_Delete),  mpc "-r")
  , ((wmod .|. shiftMask, xK_KP_Left),    mpc "-s -1")
  , ((wmod .|. shiftMask, xK_KP_Right),   mpc "-s +1")
  , ((wmod .|. shiftMask, xK_KP_Up),	  mpc "--push")
  , ((wmod .|. shiftMask, xK_KP_Down),	  mpc "--pop -s -0:15")
  , ((wmod .|. shiftMask, xK_KP_End),	  mpc "-s -0:30")
  , ((wmod .|. shiftMask, xK_KP_Next),	  mpc "-s +0:30")
  , ((mod5Mask,           xK_Left),	  mpc "-s -0:10")
  , ((mod5Mask,           xK_Right),	  mpc "-s +0:10")
  , ((mod5Mask,           xK_Down),	  mpc "-s -1")
  , ((mod5Mask,           xK_Up),	  mpc "-s +1")
  ]
  ++ zipWith (\i fk -> 
    ((0, fk),		windows $ W.view $ show i)) desktops [xK_F1..]
  ++ zipWith (\i fk -> 
    ((shiftMask, fk),	windows $ W.shift $ show i)) desktops [xK_F1..]
  ++ zipWith (\i fk -> 
    ((wmod, fk),	windows $ W.view $ show i)) desktops (xK_grave:[xK_1..])
  ++ zipWith (\i fk -> 
    ((wmod .|. shiftMask, fk),	windows $ W.shift $ show i)) desktops (xK_grave:[xK_1..])
  ++ if hostName == "pancake" then
  [ ((wmod .|. shiftMask,   xK_Prior),	spawnl ["/usr/sbin/setcx","C1"])
  , ((wmod .|. shiftMask,   xK_Next),	spawnl ["/usr/sbin/setcx","C3"])
  ] else []
  where
    mpc = spawnl . ("mpc":) . words

mouse :: [((KeyMask, Button), Window -> X ())]
mouse = --map (\(mb, rf, wf) -> (mb, \w -> isRoot w >>= \r -> if r then rf else wf w)) $
  [ ((wmod, button1),	    mouseMoveWindow)
  , ((wmod, button2),	    promptWindowOp)
  , ((wmod, button3),	    mouseResizeWindow)
  , ((wmod .|. shiftMask, button4),	    const $ mixerSet GT 1)
  , ((wmod .|. shiftMask, button5),	    const $ mixerSet LT 1)
  ]

startup :: Bool -> X ()
startup new = do
  windows $ replaceDesktopLayout iconDesktop iconLayout
  when new $ mapM_ (run . snd) startups

main :: IO ()
main = do
  args <- getArgs
  let new = "--resume" `notElem` args
  pagerLog <- pagerStart
  sct <- newEmptyMVar
  xmonad $ defaultConfig
    { normalBorderColor = colorBG
    , focusedBorderColor = colorFG
    , X.terminal = Program.terminal term
    , layoutHook = layout
    , manageHook = manager
    , handleEventHook = \e -> io (readMVar sct) >>= \c -> serverEventHook c e
    , X.workspaces = map show desktopsAll
    , modMask = wmod
    , keys = const $ Map.fromList bind
    , mouseBindings = const $ Map.fromList mouse
    , logHook = pagerLog
    , startupHook = getServerCommandType >>= io . tryPutMVar sct >> startup new
    , focusFollowsMouse = True
    }

{- TODO:
 -   bindings
 -     paste to browser
 -     paste to spellcheck
 -   menus
 -   main layouts
 -   floating/layering: in layout
 -   better resize
 -   mpc/dzen status
 -   transparent/root dzen
 -}
