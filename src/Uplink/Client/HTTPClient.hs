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

import qualified Asset
import qualified RPC
import qualified Uplink.Client as U
import qualified Uplink.Client.Config as Cfg
import qualified Uplink.Client.Version as Version

withHTTPClient :: Cfg.Config -> (U.Handle -> IO (U.Item a)) -> IO (U.Item a)
withHTTPClient cfg f = f U.Handle
  { U.config                 = cfg
  , U.createAccount          = createAccount cfg
  , U.createAsset            = createAsset cfg
  , U.createContract         = createContract cfg
  , U.getAccount             = getAccount cfg
  , U.getAccounts            = getAccounts cfg
  , U.getAsset               = getAsset cfg
  , U.getAssets              = getAssets cfg
  , U.getBlock               = getBlock cfg
  , U.getBlocks              = getBlocks cfg
  , U.getContract            = getContract cfg
  , U.getContracts           = getContracts cfg
  , U.getInvalidTransactions = getInvalidTransactions cfg
  , U.getMempool             = getMemPool cfg
  , U.getPeers               = getPeers cfg
  , U.getPoolSize            = getPoolSize cfg
  , U.getTransactions        = getTransactions cfg
  , U.getValidators          = getValidators cfg
  , U.getVersion             = getVersion cfg
  }

createAccount :: Cfg.Config -> U.Cmd -> IO (U.Item RPC.RPCResponse)
createAccount = execute

createAsset :: Cfg.Config -> U.Cmd -> IO (U.Item RPC.RPCResponse)
createAsset = execute

createContract :: Cfg.Config -> U.Cmd -> IO (U.Item RPC.RPCResponse)
createContract = execute

getAccount :: Cfg.Config -> U.Path -> IO (U.Item U.Account)
getAccount = view

getAccounts :: Cfg.Config -> U.Path -> IO (U.Item [U.Account])
getAccounts = view

getAssets :: Cfg.Config -> U.Path -> IO (U.Item [U.AssetAddress])
getAssets = view

getBlocks :: Cfg.Config -> U.Path -> IO (U.Item [U.Block])
getBlocks = view

getBlock :: Cfg.Config -> U.Path -> IO (U.Item U.Block)
getBlock = view

getAsset :: Cfg.Config -> U.Path -> IO (U.Item Asset.Asset)
getAsset = view

getContract :: Cfg.Config -> U.Path -> IO (U.Item U.Contract)
getContract = view

getContracts :: Cfg.Config -> U.Path -> IO (U.Item [U.Contract])
getContracts = view

getMemPool :: Cfg.Config -> U.Path -> IO (U.Item U.Mempool)
getMemPool = view

getPeers :: Cfg.Config -> U.Path -> IO (U.Item [U.Peer])
getPeers = view

getPoolSize :: Cfg.Config -> U.Path -> IO (U.Item U.Size)
getPoolSize = viewRaw

getTransactions :: Cfg.Config -> U.Path -> IO (U.Item [U.Transaction])
getTransactions = view

getInvalidTransactions :: Cfg.Config -> U.Path -> IO (U.Item [U.InvalidTransaction])
getInvalidTransactions = view

getValidators :: Cfg.Config -> U.Path -> IO (U.Item [U.Peer])
getValidators = view

getVersion :: Cfg.Config -> U.Path -> IO (U.Item Version.Version)
getVersion = viewRaw

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
