module Main ( main ) where

import Language.Hi.Parser

import A
import B

main = do iface <- readIface "Main.hi"
          putStrLn (showIface iface)
