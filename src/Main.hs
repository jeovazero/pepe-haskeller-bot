module Main where

import Bot (startBot)
import qualified Env
import Network.HTTP.Req (defaultHttpConfig, runReq)

initialOffset :: Int
initialOffset = 0

main :: IO ()
main = do
  putStrLn "STARTING BOT..."
  env <- Env.getEnv
  runReq defaultHttpConfig $ startBot env initialOffset
