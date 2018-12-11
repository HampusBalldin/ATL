module Main where

import Control.Monad
import Programs
import NaturalSemantics

main :: IO ()
main = do
  forM_ programs $ \p -> do
    putStrLn "--------------------------------"
    putStrLn $ show p
    putStrLn "--------------------------------"
    v <- runEval p
    putStrLn (show v)
    putStrLn "--------------------------------"
