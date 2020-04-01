{-# LANGUAGE OverloadedStrings #-}

module App.Complex
  ( Employee
  , Manager
  , Personnel
  , parse
  ) where

{- An example of handling more complex JSON data. -}

import Data.Aeson
import qualified Data.ByteString.Lazy as B

-- The technique is the same. Build records to represent objects.
-- Nest them, just like the JSON data is nested.
-- Make these things instances of 'FromJSON' and 'ToJSON' type classes,
-- and define the relevant 'parseJSON'/'toJSON' functions.

-- Here is a record to represent an employee.
data Employee = Employee
  { employeeIdent :: Int
  , name :: String
  , age :: Int
  } deriving (Show)

-- Define how to parse the record from JSON.
instance FromJSON Employee where
  parseJSON = withObject "Employee" $ \o -> do
    employeeIdent_ <- o .: "id"
    name_ <- o .: "name"
    age_ <- o .: "age"
    return Employee
      { employeeIdent = employeeIdent_
      , name = name_
      , age = age_
      }

-- Define how to encode an 'Employee' into JSON.
instance ToJSON Employee where
  toJSON employee = object
      [ "firstName" .= toJSON (employeeIdent employee)
      , "lastName" .= toJSON (name employee)
      , "age" .= toJSON (age employee)
      ]

-- Here is a type to represent a manager.
data Manager = Manager
  { managerIdent :: Int
  , department :: String
  } deriving (Show)

-- Define how to parse/encode it.
-- The '<$>' and '<*>' are just fancy sugar for what we did before.
-- The first one should be '<$>', and all other fields after that
-- should be '<*>' (which sort of means 'apply again').
instance FromJSON Manager where
  parseJSON = withObject "Manager" $ \o -> Manager
      <$> o .: "id" 
      <*> o .: "department"

instance ToJSON Manager where
  toJSON manager = object
      [ "id" .= toJSON (managerIdent manager)
      , "department" .= toJSON (department manager)
      ]

-- Here's a type to represent the personnel as a whole.
data Personnel = Personnel
  { employees :: [Employee]
  , managers :: [Manager]
  } deriving (Show)

-- Define how to parse/encode it.
instance FromJSON Personnel where
  parseJSON = withObject "Personnel" $ \o -> do
    employees_ <- o .: "employees"
    managers_ <- o .: "managers"
    return Personnel { employees = employees_, managers = managers_ }

instance ToJSON Personnel where
  toJSON personnel = object
    [ "employees" .= toJSON (employees personnel)
    , "managers" .= toJSON (managers personnel)
    ]

-- Here's a type to represent all the data in the JSON document.
data Document = Document { personnel :: Personnel } deriving (Show)

instance FromJSON Document where
  parseJSON = withObject "Document" $ \o -> Document <$> o .: "personnel"

instance ToJSON Document where
  toJSON document = object [ "personnel" .= toJSON (personnel document) ]

-- Parse a JSON object into a Document record.
parse :: B.ByteString -> Either String Document
parse json =

  let r = eitherDecode json :: Either String Document
  in case r of
    Left e -> Left e
    Right r -> Right r
