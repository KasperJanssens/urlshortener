{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.ApiType where

import           Data.Proxy    (Proxy (..))
import           Data.Text     (Text)
import           Servant       ((:>), Capture, Get, JSON, NoContent, Post,
                                ReqBody)
import           Servant.API   ((:<|>))
import           Server.NewUrl (NewUrl)

--This no content looks a bit weird but we will never return anything but a redirect or an error
type ResolverApi = Capture "id" Text :> Get '[JSON] NoContent

--It's between a capture of a fragment or a Json basically (those are the easiest). Capture
--of something that is an url will be painful so a json looks the safest bet
--Returns a simple text of the id (not the full url as in that case the application needs to know its url)
type ShortenApi = ReqBody '[JSON] NewUrl :> Post '[JSON] Text

--Separate route for resolving
type ShortenerAPI =
  "shorten" :> ShortenApi :<|>
     ResolverApi

api :: Proxy ShortenerAPI
api = Proxy
