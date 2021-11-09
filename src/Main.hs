{-# LANGUAGE OverloadedStrings #-}
module Main where

import Bot (startBot, respondSingleUpdate)
import qualified Env
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import qualified Data.Text as T
import qualified Telegram.Req as Telegram
import qualified Telegram.Data as TelegramData
import Control.Monad (when)

fallbackResponse =
  responseLBS
    status200
    [("Content-Type", "application/json")]
    "{\"repository\":\"https://github.com/jeovazero/pepe-haskeller-bot\"}"

defaultResponse =
  responseLBS
    status200
    [("Content-Type", "application/json")]
    "true"

application env req respond =
  let
    webhookPath = Env.botWebhookPath env
  in
    case pathInfo req of
      (path:_) ->
        if webhookPath == path then do
          maybeUpdate <- fmap TelegramData.decodeUpdate (strictRequestBody req)
          case maybeUpdate of
            Nothing -> respond defaultResponse
            Just update -> do
              putStrLn "responding msg"
              respondSingleUpdate env update
              respond defaultResponse
        else respond fallbackResponse
      _        -> respond fallbackResponse
  

initialOffset :: Int
initialOffset = 0

main :: IO ()
main = do
  putStrLn "STARTING BOT..."
  
  env <- Env.getEnv
 
  when (Env.debugMode env) $ print env

  let webhookUrl = Env.botWebhookUrl env
  let port = Env.botPort env
  let token = Env.botToken env


  Telegram.setWebhook token webhookUrl

  if T.null webhookUrl
  then startBot env initialOffset
  else run port (application env)