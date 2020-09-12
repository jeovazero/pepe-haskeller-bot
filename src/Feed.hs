{-# LANGUAGE OverloadedStrings #-}

module Feed
  ( haskellWeekly,
  )
where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import Network.HTTP.Req
  ( (/:),
    GET (GET),
    NoReqBody (NoReqBody),
    bsResponse,
    https,
    req,
    responseBody,
  )
import qualified Xeno.DOM as X
import Req (runReq')

text :: X.Content -> T.Text
text (X.Text a) = TEnc.decodeUtf8 a
text _ = ""

extractLink :: X.Node -> Maybe T.Text
extractLink node =
  safeHead
    $ fmap text
    $ maybe [] X.contents
    $ safeHead
    $ filter ((==) "id" . X.name)
    $ maybe [] X.children
    $ safeHead -- first entry / last weekly
    $ filter ((==) "entry" . X.name)
    $ X.children node

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

haskellWeekly :: IO (Maybe T.Text)
haskellWeekly = do
  bs <- runReq' $
    req
      GET
      (https "haskellweekly.news" /: "newsletter.atom")
      NoReqBody
      bsResponse
      mempty
  let dom = X.parse (responseBody bs)
  pure $ either (const Nothing) extractLink dom
