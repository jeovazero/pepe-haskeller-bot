{-# LANGUAGE OverloadedStrings #-}
module Feed (haskellWeekly) where

import qualified Xeno.DOM as X
import Text.Pretty.Simple (pPrint)
import Network.HTTP.Req
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import Debug.Trace (traceShow)

text (X.Text a) = TEnc.decodeUtf8 a

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

safeHead [] = Nothing
safeHead (x:_) = Just x

haskellWeekly :: Req (Maybe T.Text)
haskellWeekly = do
    bs <- req
            GET
            (https "haskellweekly.news" /: "newsletter.atom")
            NoReqBody
            bsResponse
            mempty
    let dom = X.parse (responseBody bs)
    pure $ either (const Nothing) extractLink dom
