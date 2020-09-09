{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot
  ( startBot,
  )
where

import Caani (CaaniConfig (..), caani)
import Caani.Error (CaaniError (..), CaaniErrorType (..), tryIO)
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Env
import Feed (haskellWeekly)
import Network.HTTP.Req
  ( Req,
    defaultHttpConfig,
    responseBody,
    runReq,
  )
import System.Directory (removeFile)
import Telegram.Data
  ( Cmd (..),
    MessageResponse (..),
    ResponseGetUpdate (..),
    Update (..),
    User (..),
    command,
    updatesFromResponse,
  )
import Telegram.Req (getUpdates, sendMessage, sendPhoto)

internalErroMessage :: T.Text
internalErroMessage =
  T.concat
    [ "Hm :(, an internal error.\n",
      "Please report it in ",
      "https://github.com/jeovazero/pepe-haskeller-bot/issues."
    ]

startMessage :: T.Text
startMessage =
  T.concat
    [ "I'm a bot. Work in progress for new features.\n",
      "Send me a haskell code.\n",
      "Eg.:\n\n",
      "main = print \"Hello Friend\""
    ]

decodeCaaniError :: CaaniError -> CaaniConfig -> T.Text
decodeCaaniError (CaaniError err) CaaniConfig {..} =
  let (c, l) = boundary
   in case err of
        InvalidCode ->
          "You must send me a valid haskell code."
        BoundaryLimit ->
          T.concat
            [ "The code must have a max of ",
              T.pack $ show c,
              " columns and ",
              T.pack $ show l,
              " lines."
            ]
        _ -> internalErroMessage

splitInit :: [a] -> [a] -> ([a], Maybe a)
splitInit _ [] = ([], Nothing)
splitInit acc (a : []) = (reverse acc, Just a)
splitInit acc (x : xs) = splitInit xs (x : acc)

-- (chat_id, token, messageId)
-- A poor type :(
type Params = (Int, Env.BotEnv, Maybe Int)

welcomeMessage :: [User] -> T.Text
welcomeMessage [] = "Boas vindas!!!"
welcomeMessage (u : []) = T.append "Boas vindas, " $ first_name u
welcomeMessage users = T.append "Boas vindas, " usersName
  where
    (a, b) = splitInit [] $ fmap first_name users
    usersName = T.intercalate " e " [(T.intercalate ", " a), fromMaybe "" b]

helpMessage :: T.Text
helpMessage = "/lastweekly - O link da última haskell weekly"

sendMessageResponse :: T.Text -> Params -> Req ()
sendMessageResponse _text (chat_id', env, messageId) =
  let token = Env.botToken env
      payload =
        MessageResponse
          { chat_id = chat_id',
            text = _text,
            parse_mode = Just "",
            reply_to_message_id = messageId
          }
   in sendMessage token payload >> pure ()

send :: Cmd -> Params -> Req ()
send Help params = sendMessageResponse helpMessage params
send LastWeekly params = haskellWeekly
  >>= \hw ->
    sendMessageResponse (T.append "A última weekly: " $ fromMaybe "" hw) params
send (Welcome users) params =
  sendMessageResponse (welcomeMessage users) params
send Start params =
  sendMessageResponse startMessage params
send (Code code') params@(cid, Env.BotEnv {..}, Just mid) = do
  let outimage =
        concat [T.unpack botOutputDir, "/caani-", (show cid), "-", (show mid), ".png"]
      resDir = T.unpack botResourceDir
      config =
        CaaniConfig
          { fontPath = resDir ++ "/FiraCode-Medium.ttf",
            tagPath = resDir ++ "/haskell-flag.png",
            code = code',
            outPath = outimage,
            boundary = (200, 1000)
          }
  result <- liftIO $ try $ caani config
  case result of
    Right () -> do
      r <- sendPhoto botToken outimage cid
      liftIO $ print $ responseBody r
      -- try remove the image file
      _ <- liftIO $ tryIO $ removeFile outimage
      pure ()
    Left err ->
      sendMessageResponse (decodeCaaniError err config) params
send (Code _) _ = pure ()
--  sendPhotoResponse
send _ params = sendMessageResponse "Hey I don't understand :/" params

respondSingleUpdate :: Env.BotEnv -> Update -> Req Int
respondSingleUpdate env update = do
  let (chatId, messageId, cmd) = command update
  let nextOffset = pure $ update_id update
  send cmd (chatId, env, messageId)
  nextOffset

respondUpdates :: Env.BotEnv -> Req Int -> [Update] -> Req Int
respondUpdates env offset =
  foldl (\_ u -> respondSingleUpdate env u) offset

-- TODO: use ReaderT pattern for Env
startBot :: Env.BotEnv -> Int -> Req ()
startBot env offset = runReq defaultHttpConfig $ do
  let token = Env.botToken env
  responseUpdates <- getUpdates token offset
  let updates = responseBody responseUpdates :: Maybe ResponseGetUpdate
  liftIO $ print updates
  let updates' = maybe [] updatesFromResponse updates
  offset' <- respondUpdates env (pure offset) updates'
  startBot env offset'
