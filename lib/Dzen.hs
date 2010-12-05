module Dzen
  ( runDzen
  , dzenDefaultArgs
  , (^/)
  , dzenClickArea
  ) where

import System.IO
import XMonad.Util.Run
import Param
import Ops
import Server

dzenDefaultArgs :: [String]
dzenDefaultArgs =
  ["-fg",colorRootFG
  ,"-bg","#000000"
  ,"-ta","l"
  ]

dzenServerCommand :: ServerCommand -> [Int] -> String
dzenServerCommand cmd args = "xevent:" ++ serverCommandType ++ ':' : show (fromEnum cmd) ++ concatMap ((':':) . show) args

infix 6 ^/
(^/) :: String -> String -> String
(^/) c a = '^' : c ++ '(' : a ++ ")"

dzenClickArea :: Int -> ServerCommand -> [Int] -> String -> String
dzenClickArea b c a s = "ca" ^/ (show b ++ ',' : dzenServerCommand c a) ++ s ++ "^ca()"

runDzen :: [String] -> IO Handle
runDzen = spawnPipe . unwords . ("dzen2":) . map shellEscape
