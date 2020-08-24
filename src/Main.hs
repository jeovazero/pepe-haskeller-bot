{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import Feed (haskellWeekly)
import Network.HTTP.Req
import System.Environment (lookupEnv)
import Telegram.Data
  ( ResponseGetUpdate (..),
    SendMessage (..),
    Update (..),
    User (..),
    chatIdInfo,
    cmdInfo,
    newChatMembersInfo,
    updatesFromResponse,
  )
import Telegram.Req (getUpdates, sendMessage)

tokenFromEnv :: IO T.Text
tokenFromEnv = fmap (maybe "" T.pack) $ lookupEnv "BOT_TOKEN"

welcomeMessage :: User -> T.Text
welcomeMessage = T.append "Boas vindas, " . first_name

helpMessage :: T.Text
helpMessage = "/lastweekly - O link da última haskell weekly"

sendG :: (a -> T.Text) -> Params -> a -> Req ()
sendG genText (chat_id, token, messageId) arg =
  let payload =
        SendMessage
          { chat_id = chat_id,
            text = genText arg,
            parse_mode = Just "",
            reply_to_message_id = messageId
          }
   in sendMessage token payload >> pure ()

-- (chat_id, token, messageId)
type Params = (Int, T.Text, Maybe Int)

data Arg = ArgUser User | ArgText T.Text

send :: T.Text -> Params -> Arg -> Req ()
send "/help" params arg = sendG (const helpMessage) params arg
send "/lastweekly" params _ =
  haskellWeekly
    >>= sendG (T.append "A ultima weekly: " . maybe "" id) params
send "welcome" params (ArgUser user) =
  sendG welcomeMessage params user
send _ _ _ = pure ()

respondSingleUpdate :: T.Text -> Update -> Req Int
respondSingleUpdate token update = do
  let newMembers = newChatMembersInfo update
  let (messageId, cmd) = cmdInfo update
  let nextOffset = pure $ update_id update
  let cmd' = if [] == newMembers then cmd else Just "welcome"
  case (cmd', chatIdInfo update) of
    (Just "welcome", Just chatid) -> do
      let params = (chatid, token, messageId)
      sequence_ $ fmap (send "welcome" params . ArgUser) newMembers
    (Just cmd'', Just chatid) -> do
      let params = (chatid, token, messageId)
      send cmd'' params (ArgText "")
    _ -> pure ()
  nextOffset

respondUpdates :: T.Text -> Req Int -> [Update] -> Req Int
respondUpdates token offset =
  foldl (\_ u -> respondSingleUpdate token u) offset

startBot :: Int -> Req ()
startBot offset = runReq defaultHttpConfig $ do
  token <- liftIO tokenFromEnv
  responseUpdates <- getUpdates token offset
  let updates = responseBody responseUpdates :: Maybe ResponseGetUpdate
  let updates' = maybe [] updatesFromResponse updates
  offset' <- respondUpdates token (pure offset) updates'
  startBot offset'

initialOffset :: Int
initialOffset = 0

main :: IO ()
main = runReq defaultHttpConfig $ startBot initialOffset
