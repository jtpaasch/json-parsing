{-# LANGUAGE OverloadedStrings #-}

module App.Application where

{- The main application. To run it, call the 'run' function. -}

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified System.FilePath as Path

import qualified App.Object as O
import qualified App.Number as N
import qualified App.String as S
import qualified App.Array as A
import qualified App.Tuple as T
import qualified App.Complex as C

-- Get the JSON data from a file.
getJSON = B.readFile

-- This runs the app.
run :: FilePath -> IO (Either String String)
run filepath = do

  -- Get the file contents.
  json <- getJSON filepath

  -- Handle each file differently, to illustrate each case.
  case Path.takeFileName filepath of

    -- Parsing a single JSON object into a Haskell record.
    "object.json" ->
      case O.parse json of
        Left e -> return $ Left e
        Right r -> return $ Right (show r) 

    -- Parsing a single JSON number into a Haskell 'Integer'.
    "number.json" ->
      case N.parse json of
        Left e -> return $ Left e
        Right r -> return $ Right (show r)

    -- Parsing a single JSON string into a Haskell 'String'.
    "string.json" ->
      case S.parse json of
        Left e -> return $ Left e
        Right r -> return $ Right (show r)

    -- Parsing a JSON array of strings into '[String]'.
    "array.json" ->
      case A.parse json of
        Left e -> return $ Left e
        Right r -> return $ Right (show r)

    -- Parsing a JSON array into a tuple.
    "tuple.json" ->
      case T.parse json of
        Left e -> return $ Left e
        Right r -> return $ Right (show r)

    -- Parsing more complex JSON into Haskell.
    "complex.json" ->
      case C.parse json of
        Left e -> return $ Left e
        Right r -> return $ Right (show r)

    -- Anything else, we don't know what to do with it.
    _ -> return $ Left "I don't know what this file is."

