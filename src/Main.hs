module Main where

import qualified System.Environment as Environ
import qualified App.Application as App

main :: IO ()
main = do 

  -- Get the command line arguments.
  args <- Environ.getArgs

  -- We need the first one, which is a filepath to a JSON file.
  filepath <- case args of
    [] -> fail "Provide a filepath"
    (x:_) -> return x

  -- Run the app, and print the result.
  result <- App.run filepath
  case result of 
    Left e -> putStrLn e
    Right r -> putStrLn r
