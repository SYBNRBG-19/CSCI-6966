module Types (Vulnerability(..)) where

data Vulnerability = Vulnerability
  { vulnType    :: String
  , description :: String
  , location    :: String
  } deriving (Show, Eq)
