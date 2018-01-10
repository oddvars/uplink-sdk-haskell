{-# LANGUAGE OverloadedStrings #-}
module Uplink.Client.HTTPClient
  ( withHTTPClient )
  where

import           Data.Aeson
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Client
import           Network.HTTP.Types

import qualified RPC
import qualified Uplink.Client as U
import qualified Uplink.Client.Config as Cfg

withHTTPClient :: Cfg.Config -> (U.Handle -> IO (U.Item a)) -> IO (U.Item a)
withHTTPClient cfg f = f U.Handle
  { U.config                 = cfg
  , U.circulateAsset         = execute cfg
  , U.createAccount          = execute cfg
  , U.createAsset            = execute cfg
  , U.createContract         = execute cfg
  , U.getAccount             = view cfg
  , U.getAccounts            = view cfg
  , U.getAsset               = view cfg
  , U.getAssets              = view cfg
  , U.getBlock               = view cfg
  , U.getBlocks              = view cfg
  , U.getContract            = view cfg
  , U.getContractCallable    = view cfg
  , U.getContracts           = view cfg
  , U.getInvalidTransactions = view cfg
  , U.getMempool             = view cfg
  , U.getPeers               = view cfg
  , U.getPoolSize            = view cfg
  , U.getTransactions        = view cfg
  , U.getValidators          = view cfg
  , U.getVersion             = viewRaw cfg
  , U.revokeAccount          = execute cfg
  , U.transferAsset          = execute cfg
  }

execute :: Cfg.Config -> U.Cmd -> IO (U.Item RPC.RPCResponse)
execute cfg cmd = do
  man <- newManager defaultManagerSettings
  initReq <- parseRequest (Cfg.host cfg)
  let req = initReq { method = "POST" , requestBody = RequestBodyLBS (encode cmd)}
  res <- httpLbs req man
  print res
  return $
    if responseStatus res == status200 then
      case eitherDecode . responseBody $ res of
        Left e  -> Left (T.pack e)
        Right r -> Right r
      else
        Left (T.decodeUtf8 . statusMessage . responseStatus $ res)

viewRaw :: FromJSON a => Cfg.Config -> U.Path -> IO (U.Item a)
viewRaw cfg p = do
  man <- newManager defaultManagerSettings
  initReq <- parseRequest (Cfg.host cfg)
  res <- httpLbs (initReq { method = "POST", path = U.unpath p }) man
  return $
    if responseStatus res == status200 then
      first T.pack (eitherDecode . responseBody $ res)
    else
      Left "error"

  --return $ handleResult res

view :: FromJSON a => Cfg.Config -> U.Path -> IO (U.Item a)
view cfg p = do
  man <- newManager defaultManagerSettings
  initReq <- parseRequest (Cfg.host cfg)
  res <- httpLbs (initReq { method = "POST", path = U.unpath p }) man
  return $ handleResult res

handleResult :: FromJSON a => Response BSL.ByteString -> U.Item a
handleResult res =
    if responseStatus res == status200 then
      parse . eitherDecode . responseBody $ res
    else
      Left (T.decodeUtf8 . statusMessage . responseStatus $ res)

parse :: FromJSON a => Either String RPC.RPCResponse -> U.Item a
parse (Right (RPC.RPCRespError e)) = Left (T.pack . show $ e)
parse (Right (RPC.RPCResp a))      =
  case fromJSON a of
    Success res -> Right res
    Error s     -> Left $ T.pack s
parse (Left e)                     = Left (T.pack e)
parse (Right RPC.RPCRespOK)        = Left "Error: hit RPCRespOk"
