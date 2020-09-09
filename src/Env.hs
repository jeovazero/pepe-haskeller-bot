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
        botResourceDir :: T.Text
      }
  deriving (Show)

lookupEnvText :: String -> IO T.Text
lookupEnvText = fmap (maybe "" T.pack) . lookupEnv

tokenFromEnv :: IO T.Text
tokenFromEnv = lookupEnvText "BOT_TOKEN"

outputDirFromEnv :: IO T.Text
outputDirFromEnv = lookupEnvText "BOT_OUTPUT_DIR"

resourceDirFromEnv :: IO T.Text
resourceDirFromEnv = lookupEnvText "BOT_RESOURCE_DIR"

getEnv :: IO BotEnv
getEnv = do
  botToken <- tokenFromEnv
  botOutputDir <- outputDirFromEnv
  botResourceDir <- resourceDirFromEnv
  pure $ BotEnv {..}
