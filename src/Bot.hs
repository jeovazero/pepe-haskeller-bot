{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot
  ( startBot,
  )
where

import Caani (CaaniConfig (..), caani)
import Caani.Error (CaaniError (..), CaaniErrorType (..))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Env
import Feed (haskellWeekly)
import Network.HTTP.Req ( responseBody)
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

internalErrorMessage :: T.Text
internalErrorMessage =
  T.concat
    [ "Hm :(, an internal error.\n",
      "Please, report it in ",
      "https://github.com/jeovazero/pepe-haskeller-bot/issues."
    ]

aboutMessage :: T.Text
aboutMessage =
  T.concat
    [ "I was implemented in Haskell, check my source code:\n"
    , "https://github.com/jeovazero/pepe-haskeller-bot"
    ]

startMessage :: T.Text
startMessage =
  T.concat
    [ "I'm a haskeller bot. Send me a haskell code and I ",
      "will send a beautiful image of your code.\n\n",
      ">> Work in progress for new features ;).\n\n",
      "Try it:\n\n",
      "main = print \"Hello Friend\""
    ]

decodeCaaniError :: CaaniError -> CaaniConfig -> T.Text
decodeCaaniError (CaaniError err) CaaniConfig {..} =
  let (col, lin) = boundary
  in case err of
        InvalidCode ->
          "You must send me a valid haskell code."
        BoundaryLimit ->
          T.concat
            [ "The code must have a max of ",
              T.pack $ show col,
              " columns and ",
              T.pack $ show lin,
              " lines."
            ]
        _ -> internalErrorMessage

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
helpMessage = T.unlines [
  "/lastweekly - The link of the last haskell weekly",
  "/about - Information about the bot",
  "/start - The presentation message for the PM"]

sendMessageResponse :: T.Text -> Params -> IO ()
sendMessageResponse _text (chat_id', env, messageId) =
  let token = Env.botToken env
      payload =
        MessageResponse
          { chat_id = chat_id',
            text = _text,
            parse_mode = Just "",
            reply_to_message_id = messageId
          }
   in sendMessage token payload

send ::Cmd -> Params -> IO ()
send Help params = sendMessageResponse helpMessage params
send LastWeekly params = haskellWeekly
  >>= \hw ->
    sendMessageResponse (T.append "The last Haskell Weekly: " $ fromMaybe "" hw) params
send (Welcome users) params =
  sendMessageResponse (welcomeMessage users) params
send About params = sendMessageResponse aboutMessage params
send Start params =
  sendMessageResponse startMessage params
send (Code code') params@(cid, Env.BotEnv {..}, Just mid) = do
  let outimage =
        concat [T.unpack botOutputDir, "/caani-", (show cid), "-", (show mid), ".png"]
      resDir = T.unpack botResourceDir
      -- It will create a image
      config =
        CaaniConfig
          { fontPath = resDir ++ "/FiraCode/FiraCode-Medium.ttf",
            tagPath = resDir ++ "/haskell-flag.png",
            code = code',
            outPath = outimage,
            -- code with 200 columns and 1k of lines
            boundary = (200, 1000)
          }
  result <- caani config
  case result of
    Right () -> do
      either_resp <- sendPhoto botToken outimage cid
      -- handling the sendPhoto (this request is dangerous :O)
      case either_resp of
        Left err -> print err >> sendMessageResponse internalErrorMessage params
        Right _ -> do
          -- print $ responseBody body
          -- try remove the image file
          _ <- removeFile outimage
          pure ()
    Left err -> do
      putStrLn $ concat ["> ERROR :: ", show err ]
      sendMessageResponse (decodeCaaniError err config) params
send (Code _) _ = pure ()
--  sendPhotoResponse
send _ params = sendMessageResponse "Hey I don't understand :/" params

respondSingleUpdate :: Env.BotEnv -> Update -> IO Int
respondSingleUpdate env update = do
  let (chatId, messageId, cmd) = command update
  putStrLn "> INFO :: NEW MESSAGE"
  let nextOffset = pure $ update_id update
  send cmd (chatId, env, messageId)
  nextOffset

respondUpdates :: Env.BotEnv -> IO Int -> [Update] -> IO Int
respondUpdates env offset =
  foldl (\_ u -> respondSingleUpdate env u) offset

-- TODO: use ReaderT pattern for the Env
startBot :: Env.BotEnv -> Int -> IO ()
startBot env offset = do
  let token = Env.botToken env
  responseUpdates <- getUpdates token offset
  let updates = responseBody responseUpdates :: Maybe ResponseGetUpdate
  let updates' = maybe [] updatesFromResponse updates
  offset' <- respondUpdates env (pure offset) updates'
  startBot env offset'