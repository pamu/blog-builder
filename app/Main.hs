module Main where

import           System.Environment (getArgs)

import           Validation        (validateAndProcess)

main :: IO ()
main = do
  args <- getArgs
  validateAndProcess args
