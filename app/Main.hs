--------------------------------------------------------------------------------
module Main where

import qualified Server.Core as S
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "[~] Running Exchange on localhost:8080"
  S.main
