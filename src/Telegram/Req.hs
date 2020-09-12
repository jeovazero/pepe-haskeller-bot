{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram.Req
  ( sendMessage,
    sendPhoto,
    getUpdates,
    answerInlineQuery,
    runReq'
  )
where

import Data.Text (Text, append)
import Data.Text as T
import Data.Text.Encoding as TE
import qualified Network.HTTP.Client.MultipartFormData as LM
import Network.HTTP.Req
import Telegram.Data (InlineResponse, MessageResponse, ResponseGetUpdate)
import Req (tryReq, Url', runReq')

urlGetUpdate :: Text -> Url'
urlGetUpdate token =
  https "api.telegram.org" /: (append "bot" token) /: "getUpdates"

urlSendMensage :: Text -> Url'
urlSendMensage token =
  https "api.telegram.org" /: (append "bot" token) /: "sendMessage"

urlSendPhoto :: Text -> Url'
urlSendPhoto token =
  https "api.telegram.org" /: (append "bot" token) /: "sendPhoto"

urlAnswerInlineQuery :: Text -> Url'
urlAnswerInlineQuery token =
  https "api.telegram.org" /: (append "bot" token) /: "answerInlineQuery"

sendMessage :: Text -> MessageResponse -> IO ()
sendMessage token payload =
  runReq'
    $ req POST (urlSendMensage token) (ReqBodyJson payload) ignoreResponse mempty
    >> pure ()

sendPhoto :: Text -> String -> Int -> IO (Either HttpException BsResponse)
sendPhoto token file cid = do
  body <-
    reqBodyMultipart
      [ LM.partBS "chat_id" (TE.encodeUtf8 $ T.pack $ show cid),
        LM.partFileSource "photo" file
      ]
  tryReq $ req POST (urlSendPhoto token) body bsResponse mempty

getUpdates :: Text -> Int -> IO (JsonResponse (Maybe ResponseGetUpdate))
getUpdates token offset =
  runReq' $ req GET (urlGetUpdate token) NoReqBody jsonResponse $
    "timeout" =: (3600 :: Int)
      <> "offset" =: (offset + 1)
      <> responseTimeout 3601000000

answerInlineQuery :: Text -> InlineResponse -> IO BsResponse
answerInlineQuery token payload = do
  runReq' $ req POST (urlAnswerInlineQuery token) (ReqBodyJson payload) bsResponse mempty
