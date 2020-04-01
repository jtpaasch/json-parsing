{-# LANGUAGE OverloadedStrings #-}

module App.Tuple (parse) where

{- An example of handling a JSON array with distinct data types. -}

import Data.Aeson
import qualified Data.ByteString.Lazy as B

-- How do we parse a JSON array into a Haskell value, when the members
-- of the array are different in kind? We have to use tuples.

-- If the members of the list are simple objects (numbers, strings etc.)
-- then the 'FromJSON' type class is smart enough to handle it.

-- Parse a JSON object into a string.
parse :: B.ByteString -> Either String (String, Integer, (Bool, Bool))
parse json =

  -- To decode, we can use 'decode', which returns 'Maybe', which gives
  -- us 'Nothing' if something goes wrong, or we can use 'eitherDecode',
  -- which gives a string error on the left.

  -- Note how we specify the return type here as a tuple:
  -- '(String, Integer, (Bool, Bool))'.
  -- Thats' how Aeson knows to convert the list of values in the JSON
  -- data into a tuple with that structure. 
  let r = eitherDecode json :: Either String (String, Integer, (Bool, Bool))
  in case r of
    Left e -> Left e
    Right r -> Right r
