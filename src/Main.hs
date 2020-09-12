module Main where

import Bot (startBot)
import qualified Env

initialOffset :: Int
initialOffset = 0

main :: IO ()
main = do
  putStrLn "STARTING BOT..."
  env <- Env.getEnv
  startBot env initialOffset