module Domain.ShortenedUrl where

import           Data.Text (Text)

newtype ShortenedUrl = ShortenedUrl {url :: Text}
