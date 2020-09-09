{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram.Req
  ( sendMessage,
    sendPhoto,
    getUpdates,
    answerInlineQuery,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (encode)
import Data.Text (Text, append)
import Data.Text as T
import Data.Text.Encoding as TE
import qualified Network.HTTP.Client.MultipartFormData as LM
import Network.HTTP.Req
import Telegram.Data (InlineResponse, MessageResponse, ResponseGetUpdate)

urlGetUpdate :: Text -> Url 'Https
urlGetUpdate token =
  https "api.telegram.org" /: (append "bot" token) /: "getUpdates"

urlSendMensage :: Text -> Url 'Https
urlSendMensage token =
  https "api.telegram.org" /: (append "bot" token) /: "sendMessage"

urlSendPhoto :: Text -> Url 'Https
urlSendPhoto token =
  https "api.telegram.org" /: (append "bot" token) /: "sendPhoto"

urlAnswerInlineQuery :: Text -> Url 'Https
urlAnswerInlineQuery token =
  https "api.telegram.org" /: (append "bot" token) /: "answerInlineQuery"

sendMessage :: Text -> MessageResponse -> Req IgnoreResponse
sendMessage token payload =
  req POST (urlSendMensage token) (ReqBodyJson payload) ignoreResponse mempty

sendPhoto :: Text -> String -> Int -> Req BsResponse
sendPhoto token file cid = do
  body <-
    reqBodyMultipart
      [ LM.partBS "chat_id" (TE.encodeUtf8 $ T.pack $ show cid),
        LM.partFileSource "photo" file
      ]
  req POST (urlSendPhoto token) body bsResponse mempty

getUpdates :: (MonadHttp m) => Text -> Int -> m (JsonResponse (Maybe ResponseGetUpdate))
getUpdates token offset =
  req GET (urlGetUpdate token) NoReqBody jsonResponse $
    "timeout" =: (3600 :: Int)
      <> "offset" =: (offset + 1)
      <> responseTimeout 3601000000

answerInlineQuery :: Text -> InlineResponse -> Req BsResponse
answerInlineQuery token payload = do
  liftIO $ print $ encode payload
  r <- req POST (urlAnswerInlineQuery token) (ReqBodyJson payload) bsResponse mempty
  liftIO $ print $ responseBody r
  pure r
