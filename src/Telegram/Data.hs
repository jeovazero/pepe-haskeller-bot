{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Data
  ( ResponseGetUpdate (..),
    SendMessage (..),
    Update (..),
    Message (..),
    User (..),
    newChatMembersInfo,
    updatesFromResponse,
    cmdInfo,
    chatIdInfo,
  )
where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text, split)
import GHC.Generics (Generic)

data ResponseGetUpdate
  = ResponseGetUpdate
      { ok :: Bool,
        result :: Maybe [Update]
      }
  deriving (Show)

instance FromJSON ResponseGetUpdate where
  parseJSON (Object obj) =
    ResponseGetUpdate <$> obj .: "ok"
      <*> obj .:? "result"
  parseJSON invalid = typeMismatch "Object" invalid

data Update
  = Update
      { update_id :: Int,
        message :: Maybe Message
      }
  deriving (Show)

instance FromJSON Update where
  parseJSON (Object obj) =
    Update <$> obj .: "update_id"
      <*> obj .:? "message"
  parseJSON invalid = typeMismatch "Object" invalid

data Message
  = Message
      { message_id :: Int,
        from :: Maybe User,
        chat :: Chat,
        date :: Int,
        message_text :: Maybe Text,
        new_chat_members :: Maybe [User]
      }
  deriving (Show)

instance FromJSON Message where
  parseJSON (Object obj) =
    Message <$> obj .: "message_id"
      <*> obj .:? "from"
      <*> obj .: "chat"
      <*> obj .: "date"
      <*> obj .:? "text"
      <*> obj .:? "new_chat_members"
  parseJSON invalid = typeMismatch "Object" invalid

data User
  = User
      { user_id :: Integer,
        is_bot :: Bool,
        first_name :: Text,
        last_name :: Maybe Text,
        username :: Maybe Text
      }
  deriving (Show, Eq)

instance FromJSON User where
  parseJSON (Object obj) =
    User <$> obj .: "id"
      <*> obj .: "is_bot"
      <*> obj .: "first_name"
      <*> obj .:? "last_name"
      <*> obj .:? "username"
  parseJSON invalid = typeMismatch "Object" invalid

data Chat
  = Chat
      { _chat_id :: Int,
        _type :: Text,
        title :: Maybe Text,
        username :: Maybe Text,
        first_name :: Maybe Text,
        last_name :: Maybe Text
      }
  deriving (Show)

instance FromJSON Chat where
  parseJSON (Object obj) =
    Chat <$> obj .: "id"
      <*> obj .: "type"
      <*> obj .:? "title"
      <*> obj .:? "username"
      <*> obj .:? "first_name"
      <*> obj .:? "last_name"
  parseJSON invalid = typeMismatch "Object" invalid

data SendMessage
  = SendMessage
      { chat_id :: Int,
        text :: Text,
        parse_mode :: Maybe Text,
        reply_to_message_id :: Maybe Int
      }
  deriving (Generic, Show)

instance ToJSON SendMessage

updatesFromResponse :: ResponseGetUpdate -> [Update]
updatesFromResponse resp = maybe [] id $ result resp

newChatMembersInfo :: Update -> [User]
newChatMembersInfo =
  maybe [] (maybe [] id . new_chat_members) . message

chatIdInfo :: Update -> Maybe Int
chatIdInfo = fmap (_chat_id . chat) . message

headText :: [Text] -> Text
headText [] = ""
headText (x : _) = x

getCmd :: Text -> Text
getCmd = headText . split (== '@')

cmdInfo :: Update -> (Maybe Int, Maybe Text)
cmdInfo u =
  maybe
    (Nothing, Nothing)
    (\m -> (Just $ message_id m, fmap getCmd $ message_text m))
    $ message u
