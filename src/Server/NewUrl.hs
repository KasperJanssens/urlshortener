{-# LANGUAGE DeriveGeneric #-}

module Server.NewUrl where

import           Data.Aeson.Types (FromJSON, ToJSON)
import           Data.Text        (Text)
import           GHC.Generics     (Generic)

newtype NewUrl = NewUrl {url :: Text}
  deriving (Generic, Show)

instance ToJSON NewUrl

instance FromJSON NewUrl
