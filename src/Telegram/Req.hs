{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram.Req
  ( sendMessage,
    getUpdates,
  )
where

import Data.Text (Text, append)
import Network.HTTP.Req
import Telegram.Data (ResponseGetUpdate, SendMessage)

urlGetUpdate :: Text -> Url 'Https
urlGetUpdate token =
  https "api.telegram.org" /: (append "bot" token) /: "getUpdates"

urlSendMensage :: Text -> Url 'Https
urlSendMensage token =
  https "api.telegram.org" /: (append "bot" token) /: "sendMessage"

sendMessage :: Text -> SendMessage -> Req IgnoreResponse
sendMessage token payload =
  req POST (urlSendMensage token) (ReqBodyJson payload) ignoreResponse mempty

getUpdates :: (MonadHttp m) => Text -> Int -> m (JsonResponse (Maybe ResponseGetUpdate))
getUpdates token offset =
  req GET (urlGetUpdate token) NoReqBody jsonResponse $
    "timeout" =: (3600 :: Int)
      <> "offset" =: (offset + 1)
      <> responseTimeout 3601000000
