{-# LANGUAGE OverloadedStrings #-}

module App.Array (parse) where

{- An example of handling a single JSON string. -}

import Data.Aeson
import qualified Data.ByteString.Lazy as B

-- How do we parse a JSON array into a Haskell List?

-- The 'FromJSON' type class provides utilities for parsing JSON,
-- so long as you define the 'parseJSON' method for it.
-- It might be tempting to do that for an array.

-- If the array contains members of the same type, and those types
-- are simple (like strings, or bools) rather than complex (like objects),
-- then the 'FromJSON' type class already defines how to do it.

-- Parse a JSON object into a string.
parse :: B.ByteString -> Either String [String]
parse json =

  -- To decode, we can use 'decode', which returns 'Maybe', which gives
  -- us 'Nothing' if something goes wrong, or we can use 'eitherDecode',
  -- which gives a string error on the left.

  -- Note how we specify the return type here as '[String]'.
  -- Thats' how Aeson knows to convert the list of values in the JSON
  -- data into a list of Haskell strings.
  let r = eitherDecode json :: Either String [String]
  in case r of
    Left e -> Left e
    Right r -> Right r
