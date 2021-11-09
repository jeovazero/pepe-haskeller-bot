{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Data
  ( ResponseGetUpdate (..),
    MessageResponse (..),
    PhotoResponse (..),
    InlineResponse (..),
    InlinePhotoResponse (..),
    Update (..),
    Message (..),
    User (..),
    Cmd (..),
    Command,
    decodeUpdate,
    newChatMembersInfo,
    updatesFromResponse,
    command,
    chatIdInfo,
  )
where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text, isPrefixOf, split)
import GHC.Generics (Generic)
import Data.ByteString.Lazy (ByteString)

data ResponseGetUpdate
  = ResponseGetUpdate
      { ok :: Bool,
        result :: Maybe [Update]
      }
  deriving (Show)

instance FromJSON ResponseGetUpdate where
  parseJSON (Object obj) =
    ResponseGetUpdate
      <$> obj .: "ok"
      <*> obj .:? "result"
  parseJSON invalid = typeMismatch "Object" invalid

data Update
  = Update
      { update_id :: Int,
        message :: Maybe Message,
        inline_query :: Maybe InlineQuery
      }
  deriving (Show)

instance FromJSON Update where
  parseJSON (Object obj) =
    Update
      <$> obj .: "update_id"
      <*> obj .:? "message"
      <*> obj .:? "inline_query"
  parseJSON invalid = typeMismatch "Object" invalid

data InlineQuery
  = InlineQuery
      { query_id :: Text,
        from :: Maybe User,
        query :: Text
      }
  deriving (Show)

instance FromJSON InlineQuery where
  parseJSON (Object obj) =
    InlineQuery
      <$> obj .: "id"
      <*> obj .:? "from"
      <*> obj .: "query"
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
    Message
      <$> obj .: "message_id"
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
    User
      <$> obj .: "id"
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
    Chat
      <$> obj .: "id"
      <*> obj .: "type"
      <*> obj .:? "title"
      <*> obj .:? "username"
      <*> obj .:? "first_name"
      <*> obj .:? "last_name"
  parseJSON invalid = typeMismatch "Object" invalid

data MessageResponse
  = MessageResponse
      { chat_id :: Int,
        text :: Text,
        parse_mode :: Maybe Text,
        reply_to_message_id :: Maybe Int
      }
  deriving (Generic, Show)

instance ToJSON MessageResponse

data PhotoResponse
  = PhotoResponse
      { chat_id :: Int,
        photo :: Text
      }
  deriving (Generic, Show)

instance ToJSON PhotoResponse

data InlineResponse
  = InlineResponse
      { inline_query_id :: Text,
        results :: [InlinePhotoResponse]
      }
  deriving (Generic, Show)

instance ToJSON InlineResponse

data InlinePhotoResponse
  = InlinePhotoResponse
      { _type :: Text,
        _id :: Text,
        photo_url :: Text,
        thumb_url :: Text,
        title :: Maybe Text
      }
  deriving (Generic, Show)

instance ToJSON InlinePhotoResponse where
  toJSON InlinePhotoResponse {..} =
    object
      [ "type" .= _type,
        "id" .= _id,
        "photo_url" .= photo_url,
        "thumb_url" .= thumb_url,
        "title" .= title
      ]
  toEncoding InlinePhotoResponse {..} =
    pairs
      ( "type" .= _type
          <> "id" .= _id
          <> "photo_url" .= photo_url
          <> "thumb_url" .= thumb_url
          <> "title" .= title
      )

updatesFromResponse :: ResponseGetUpdate -> [Update]
updatesFromResponse resp = maybe [] id $ result resp

newChatMembersInfo :: Message -> [User]
newChatMembersInfo =
  maybe [] id . new_chat_members

chatIdInfo :: Message -> Int
chatIdInfo = _chat_id . chat

headText :: [Text] -> Text
headText [] = ""
headText (x : _) = x

data Cmd
  = Help
  | Start
  | About
  | Welcome [User]
  | LastWeekly
  | Code Text
  | Unknown
  deriving (Show)

type Command = (Int, Maybe Int, Cmd)

getCmdFromText :: Text -> Cmd
getCmdFromText t =
  case headText $ split (== '@') t of
    "/lastweekly" -> LastWeekly
    "/help" -> Help
    "/start" -> Start
    "/about" -> About
    _ ->
      if isPrefixOf "/" t
        then Unknown
        else Code t

empty' :: [a] -> Bool
empty' [] = True
empty' _ = False

command :: Update -> (Int, Maybe Int, Cmd)
command u =
  case message u of
    Nothing -> (0, Nothing, Unknown)
    Just msg -> (chatIdInfo msg, Just m_id, cmd)
      where
        m_id = message_id msg
        nchat = newChatMembersInfo msg
        cmdFromText = maybe Unknown getCmdFromText $ message_text msg
        cmd =
          if empty' nchat
            then cmdFromText
            else Welcome nchat

decodeUpdate :: ByteString -> Maybe Update
decodeUpdate = decode