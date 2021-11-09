{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Env
  ( getEnv,
    BotEnv (..),
  )
where

import qualified Data.Text as T
import System.Environment (lookupEnv)

data BotEnv
  = BotEnv
      { botToken :: T.Text,
        botOutputDir :: T.Text,
        botResourceDir :: T.Text,
        botWebhookUrl :: T.Text,
        botWebhookPath :: T.Text,
        botPort :: Int,
        debugMode :: Bool
      }
  deriving (Show)

lookupEnvText :: T.Text -> String -> IO T.Text
lookupEnvText defaultValue  = fmap (maybe defaultValue T.pack) . lookupEnv

tokenFromEnv :: IO T.Text
tokenFromEnv = lookupEnvText "" "BOT_TOKEN"

outputDirFromEnv :: IO T.Text
outputDirFromEnv = lookupEnvText "./" "BOT_OUTPUT_DIR"

resourceDirFromEnv :: IO T.Text
resourceDirFromEnv = lookupEnvText "./resources" "BOT_RESOURCE_DIR"

webhookFromEnv :: IO T.Text
webhookFromEnv = lookupEnvText "" "BOT_WEBHOOK_BASE_URL"

webhookPathFromEnv :: IO T.Text
webhookPathFromEnv = lookupEnvText "telegram" "BOT_WEBHOOK_PATH"

portFromEnv :: IO Int
portFromEnv = fmap (maybe 80 read) $ lookupEnv "PORT"

debugModeFromEnv :: IO Bool
debugModeFromEnv = fmap (maybe False (\x -> x /= "" && x /= "0")) $ lookupEnv "DEBUG"

getEnv :: IO BotEnv
getEnv = do
  botToken <- tokenFromEnv
  botOutputDir <- outputDirFromEnv
  botResourceDir <- resourceDirFromEnv
  botWebhookPath <- webhookPathFromEnv

  webhook <- webhookFromEnv
  let botWebhookUrl = if T.null webhook
                      then webhook
                      else T.concat [webhook, "/", botWebhookPath]
  debugMode <- debugModeFromEnv
  botPort <- portFromEnv
  pure $ BotEnv {..}
