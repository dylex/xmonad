{-# LANGUAGE FlexibleContexts #-}
import XMonad as X hiding (mouseResizeWindow)
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.CopyWindow as XCW
import XMonad.Actions.CycleWS
import XMonad.Actions.FlexibleResize
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.Column
import XMonad.Layout.NoBorders
import XMonad.Util.Run
import XMonad.Util.Types
import XMonad.Util.WindowProperties
import Control.Monad
import qualified Data.Map as Map
import Data.Monoid
import Data.Ratio ((%))
import System.Environment
import System.Exit
import Graphics.X11.ExtraTypes.XF86
import Param
import Ops
import Layout
import Pager
import Server
import Program
import Prompt
import Selection

isStuck :: Property
isStuck = Title "stuck term"

layout = lessBorders OnlyFloat $ splitLayout (R, 8+80*6) isStuck lmain lstuck
  where
  lmain = Full ||| Tall 1 (1%32) (1%2) ||| Column 1
  lstuck = Column 1

iconLayout = Tall 1 (1%32) (1%2) -- FIXME

manager :: ManageHook
manager = composeAll
  [ isElem title ["Stripchart","xeyes","xload","xdaliclock","Dali Clock","xrtail"] --> doIgnore
  , isElem className ["feh","Gimp","xmag"] <||> isElem title ["Event Tester","MPlayer","2by2"] --> doFloat
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
  , ((wmod .|. shiftMask,   xK_f),	withSelection $ runBrowser . Just)
  , ((wmod .|. shiftMask,   xK_g),	withSelection $ \u -> run $ Run "elinks" ["-remote",u])
  , ((wmod,		    xK_c),	windows (W.swapUp . W.focusDown))
  , ((wmod .|. shiftMask,   xK_c),	promptRun False)
  , ((wmod,		    xK_r),	windows (W.shiftMaster . W.focusUp))
  , ((wmod .|. shiftMask,   xK_r),	promptLogin)
  , ((wmod,		    xK_l),	runTerm term)
  , ((wmod .|. shiftMask,   xK_l),	promptRun True)
  , ((wmod .|. controlMask, xK_l),	setLayout (Layout layout) >> refresh)
  , ((wmod,		    xK_slash),	sendMessage NextLayout)
  , ((wmod .|. shiftMask,   xK_slash),	sendMessage FirstLayout)
  , ((wmod .|. controlMask, xK_slash),	run $ RunShell "xclip -o | aspell -a | grep '^&' | xmessage -file -")
  , ((wmod,		    xK_equal),	sendMessage Expand)
  , ((wmod .|. shiftMask,   xK_equal),	sendMessage Shrink)
  -- xK_backslash

  -- xK_a
  -- xK_o
  -- xK_e
  -- xK_u
  , ((wmod,		    xK_i),	withFocused (\w -> io $ runProcessWithInput "xprop" ["-id",show w] "" >>= notify))
  --, ((wmod .|. shiftMask,   xK_i),	runLogin "icicle")
  , ((wmod,		    xK_d),	sendMessage SwitchFocus) -- nextScreen
  , ((wmod .|. shiftMask,   xK_d),	runLogin "dylex")
  , ((wmod,		    xK_h),	windows $ viewDesk predWrap)
  , ((wmod .|. shiftMask,   xK_h),	windows $ viewDesk predWrap . shiftDesk predWrap)
  , ((wmod,		    xK_t),	sendMessage $ SplitModifies W.focusUp')
  , ((wmod,		    xK_n),	sendMessage $ SplitModifies W.focusDown')
  , ((wmod,		    xK_s),	windows $ viewDesk succWrap)
  , ((wmod .|. shiftMask,   xK_s),	windows $ viewDesk succWrap . shiftDesk succWrap)
  , ((wmod,		    xK_minus),	toggleWS)
  , ((wmod .|. shiftMask,   xK_minus),	withFocused (sendMessage . SwitchWindow))
  , ((wmod,		    xK_Return),	windows $ W.view $ show $ head desktops)

  , ((wmod,		    xK_semicolon), run $ RunShell $ (if hostHome then "" else "xlock && ") ++ "sleep 2 && xset dpms force off")
  , ((wmod .|. shiftMask,   xK_semicolon), promptOp)
  , ((wmod,		    xK_q),	run (Run "xlock" []))
  -- j
  -- k
  , ((wmod,		    xK_x),	kill)
  , ((wmod,		    xK_b),	run $ RunShell "xbg && [ -p HOME/.xtail ] && touch HOME/.xtail")
  , ((wmod,		    xK_m),	withFocused floatAdjust)
  , ((wmod .|. shiftMask,   xK_m),	withFocused (windows . W.sink))
  , ((wmod,		    xK_w),	windows W.shiftMaster)
  , ((wmod .|. shiftMask,   xK_w),	withFocused promptWindowOp)
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

  , ((wmod .|. shiftMask, xK_Prior),	run $ if hostName == "pancake" then Run "/usr/sbin/setcx" ["C1"] else Run "eject" [])
  , ((wmod .|. shiftMask, xK_Next),	run $ if hostName == "pancake" then Run "/usr/sbin/setcx" ["C3"] else Run "eject" ["-t"])
  ]
  ++ zipWith (\i fk -> 
    ((0, fk),		windows $ W.view $ show i)) desktops [xK_F1..]
  ++ zipWith (\i fk -> 
    ((shiftMask, fk),	windows $ W.shift $ show i)) desktops [xK_F1..]
  ++ zipWith (\i fk -> 
    ((wmod, fk),	windows $ W.view $ show i)) desktops (xK_grave:[xK_1..])
  ++ zipWith (\i fk -> 
    ((wmod .|. shiftMask, fk),	windows $ W.shift $ show i)) desktops (xK_grave:[xK_1..])
  where
    mpc = run . Run "mpc" . words

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
  updateLayout (show iconDesktop) $ Just $ Layout iconLayout
  when new $ mapM_ (run . snd) startups

main :: IO ()
main = do
  args <- getArgs
  let new = "--resume" `notElem` args
  pagerLog <- pagerStart
  xmonad XConfig
    { normalBorderColor = "#6060A0"
    , focusedBorderColor = "#E0E0A0"
    , X.terminal = Program.terminal term
    , layoutHook = layout
    , manageHook = manager
    , handleEventHook = serverEventHook `mappend` selectionEventHook
    , X.workspaces = map show desktopsAll
    , modMask = wmod
    , keys = const $ Map.fromList bind
    , mouseBindings = const $ Map.fromList mouse
    , borderWidth = 1
    , logHook = pagerLog
    , startupHook = do
	dpy <- asks display
	root <- asks theRoot
	-- add propertyNotifyMask for rootw ... for selection
	io $ selectInput dpy root $ substructureRedirectMask .|. substructureNotifyMask
			        .|. enterWindowMask .|. leaveWindowMask .|. structureNotifyMask
			        .|. buttonPressMask .|. propertyChangeMask
	setWMName "LG3D"
	serverInit
	startup new
    , focusFollowsMouse = True
    }

{- TODO:
 -   main layouts
 -   floating/layering: in layout
 -     maybe layer st focus == master?
 -   applySizeHints when float? might need to add w/h to X11 but are obsolete
 -     partly working
 -   better resize/move: display size
 -   resize issues: mrxvt/firefox start wrong size
 -   mrxvt refresh: better
 -   esc vs capslock bindings?
 -   mpc/dzen status
 -   transparent/root dzen
 -   throttle logHook: update at most every second?
 -}
