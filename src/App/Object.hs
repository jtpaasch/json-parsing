{-# LANGUAGE OverloadedStrings #-}

module App.Object
  ( Employee
  , parse
  ) where

{- An example of handling a single JSON object. -}

import Data.Aeson
import qualified Data.ByteString.Lazy as B

-- Here is a record to represent an employee.
data Employee = Employee
  { firstName :: String
  , lastName :: String
  , age :: Int
  } deriving (Show)

-- The 'FromJSON' typeclass has utilities for converting things from JSON.
-- So let's make 'Employee' an instance of 'FromJSON'.
instance FromJSON Employee where

  -- 'FromJSON' requires that we define a 'parseJSON' method,
  -- so it knows how to parse a suitable JSON object into an
  -- 'Employee' record.
  parseJSON = withObject "Employee" $ \o -> do

    -- First, pull each of the following named fields out of the object.
    -- We can do that with the infixed '.:' function. This is basically
    -- like 'HashMap.lookup "firstName" o', etc. 
    firstName_ <- o .: "firstName"
    lastName_ <- o .: "lastName"
    age_ <- o .: "age"

    -- Now build an 'Employee' record from the extracted values.
    return Employee
      { firstName = firstName_
      , lastName = lastName_
      , age = age_
      }

-- The 'ToJSON' type class has utilities for converting things to JSON.
-- So let's make 'Employee' an instance of 'ToJSON'.
instance ToJSON Employee where

  -- 'ToJSON' requires that we define a 'toJSON' method,
  -- so it knows how to convert an 'Employee' record into JSON.
  toJSON employee = 

    -- First, convert the values of the fields into JSON values.
    -- We do this by calling 'toJSON' on each of them.
    let firstName_ = toJSON (firstName employee)
        lastName_ = toJSON (lastName employee)
        age_ = toJSON (age employee)

    -- Then, convert these into a JSON object, using the 'object'
    -- function, which takes a list of key/value pairs.
    -- We can make a key/value pair with the infixed '.=' function.
    in object
      [ "firstName" .= firstName_
      , "lastName" .= lastName_
      , "age" .= age_
      ]

-- Parse a JSON object into an Employee record.
parse :: B.ByteString -> Either String Employee
parse json =

  -- Use the 'eitherDecode' function, so we can get back the error.
  -- The regular 'decode' function returns 'Nothing' if there's a problem.
  let r = eitherDecode json :: Either String Employee
  in case r of
    Left e -> Left e
    Right r -> Right r
