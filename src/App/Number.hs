{-# LANGUAGE OverloadedStrings #-}

module App.Number (parse) where

{- An example of handling a single JSON number. -}

import Data.Aeson
import qualified Data.ByteString.Lazy as B

-- How do we parse a JSON number into an integer (or a float)?
--
-- The 'FromJSON' type class provides utilities for parsing JSON,
-- so long as you define the 'parseJSON' method for it.
-- It might be tempting to do that for numbers:
--
-- class FromJSON Integer where
--   parseJSON = withNumber "Integer" (\x -> return ...)

-- But the 'FromJSON' type class already defines this. So we don't
-- need to do it ourselves. This is true for simple data types
-- like String, Bool, etc.

-- Parse a JSON object into an integer. We could also do Float.
parse :: B.ByteString -> Either String Integer
parse json =

  -- To decode, we can use 'decode', which returns 'Maybe', which gives
  -- us 'Nothing' if something goes wrong, or we can use 'eitherDecode',
  -- which gives a string error on the left.
  
  -- Note how we specify the return type here as 'Integer'.
  -- Thats' how Aeson knows to convert the value in the JSON
  -- data into a Haskell 'Integer'.

  -- Use the 'eitherDecode' function, so we can get back the error.
  -- The regular 'decode' function returns 'Nothing' if there's a problem.
  let r = eitherDecode json :: Either String Integer
  in case r of
    Left e -> Left e
    Right r -> Right r
