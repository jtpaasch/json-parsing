{-# LANGUAGE OverloadedStrings #-}

module App.String (parse) where

{- An example of handling a single JSON string. -}

import Data.Aeson
import qualified Data.ByteString.Lazy as B

-- How do we parse a JSON string into a Haskell 'String'?

-- The 'FromJSON' type class provides utilities for parsing JSON,
-- so long as you define the 'parseJSON' method for it.
-- It might be tempting to do that for strings:

-- class FromJSON String where
--   parseJSON = withString "String" (\x -> return ...)

-- But the 'FromJSON' type class already defines this. So we don't need
-- to do it ourselves. This is true for simple data types, like
-- numbers, bools, etc.

-- Parse a JSON object into a string.
parse :: B.ByteString -> Either String String
parse json =

  -- To decode, we can use 'decode', which returns 'Maybe', which gives
  -- us 'Nothing' if something goes wrong, or we can use 'eitherDecode',
  -- which gives a string error on the left.
  
  -- Note how we specify the return type here as 'String'.
  -- Thats' how Aeson knows to convert the value in the JSON
  -- data into a Haskell string.

  -- Use the 'eitherDecode' function, so we can get back the error.
  -- The regular 'decode' function returns 'Nothing' if there's a problem.
  let r = eitherDecode json :: Either String String
  in case r of
    Left e -> Left e
    Right r -> Right r
