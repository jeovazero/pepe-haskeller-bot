{-# LANGUAGE DataKinds #-}
module Req (Url', runReq', tryReq) where

import Network.HTTP.Req
import Control.Exception (try)

runReq' :: Req a -> IO a
runReq' = runReq defaultHttpConfig

tryReq :: Req a -> IO (Either HttpException a)
tryReq = try . runReq'

type Url' = Url 'Https