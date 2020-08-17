{-# LANGUAGE OverloadedStrings #-}
module Telegram.Req (sendMessage, getUpdates) where
    
import Network.HTTP.Req
import Data.Text (Text, append)
import Telegram.Data (SendMessage)

urlGetUpdate token =
    https "api.telegram.org" /: (append "bot" token) /: "getUpdates"

urlSendMensage token =
    https "api.telegram.org" /: (append "bot" token) /: "sendMessage"

sendMessage :: Text -> SendMessage -> Req IgnoreResponse
sendMessage token payload =
    req POST (urlSendMensage token) (ReqBodyJson payload) ignoreResponse mempty

getUpdates token offset =
    req GET (urlGetUpdate token) NoReqBody jsonResponse $
           "timeout" =: (3600 :: Int)
        <> "offset" =: (offset + 1)
        <> responseTimeout 3601000000


