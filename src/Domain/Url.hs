module Domain.Url where

import           Data.Text (Text)

newtype Url = Url {url :: Text} deriving (Show)
