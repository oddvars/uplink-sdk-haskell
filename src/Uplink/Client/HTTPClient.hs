{-# LANGUAGE OverloadedStrings #-}
module Uplink.Client.HTTPClient
  ( withHTTPClient
  )
  where

import           Data.Aeson
import           Data.Maybe
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
  { U.config          = cfg
  , U.createContract  = createContract cfg
  , U.getAccount      = getAccount cfg
  , U.getAccounts     = getAccounts cfg
  , U.createAccount   = createAccount cfg
  , U.getAsset        = getAsset cfg
  , U.getAssets       = getAssets cfg
  , U.createAsset     = createAsset cfg
  , U.getBlock        = getBlock cfg
  , U.getBlocks       = getBlocks cfg
  , U.getContract     = getContract cfg
  , U.getContracts    = getContracts cfg
  , U.getMempool      = getMemPool cfg
  , U.getPeers        = getPeers cfg
  , U.getTransactions = getTransactions cfg
  , U.getInvalidTransactions = getInvalidTransactions cfg
  , U.getValidators   = getValidators cfg
  , U.getVersion      = getVersion cfg
  }

getAccount :: Cfg.Config -> U.Path -> IO (U.Item U.Account)
getAccount = flip post' Nothing

getAccounts :: Cfg.Config -> U.Path -> IO (U.Item [U.Account])
getAccounts = flip post' Nothing

createAccount :: Cfg.Config -> Maybe U.Cmd -> IO (U.Item ())
createAccount cfg createAcct = post' cfg createAcct root

createAsset :: Cfg.Config -> Maybe U.Cmd -> IO (U.Item ())
createAsset cfg mCmd = post' cfg mCmd root

createContract :: Cfg.Config -> Maybe U.Cmd -> IO (U.Item ())
createContract cfg mCmd = post' cfg mCmd root

getAssets :: Cfg.Config -> U.Path -> IO (U.Item [U.AssetAddress])
getAssets = flip post' Nothing

getBlocks :: Cfg.Config -> U.Path -> IO (U.Item [U.Block])
getBlocks = flip post' Nothing

getBlock :: Cfg.Config -> U.Path -> IO (U.Item U.Block)
getBlock = flip post' Nothing

getAsset :: Cfg.Config -> U.Path -> IO (U.Item Asset.Asset)
getAsset = flip post' Nothing

getContract :: Cfg.Config -> U.Path -> IO (U.Item U.Contract)
getContract = flip post' Nothing

getContracts :: Cfg.Config -> U.Path -> IO (U.Item [U.Contract])
getContracts = flip post' Nothing

getMemPool :: Cfg.Config -> U.Path -> IO (U.Item U.Mempool)
getMemPool = flip post' Nothing

getPeers :: Cfg.Config -> U.Path -> IO (U.Item [U.Peer])
getPeers = flip post' Nothing

getTransactions :: Cfg.Config -> U.Path -> IO (U.Item [U.Transaction])
getTransactions = flip post' Nothing

getInvalidTransactions :: Cfg.Config -> U.Path -> IO (U.Item [U.InvalidTransaction])
getInvalidTransactions = flip post' Nothing

getValidators :: Cfg.Config -> U.Path -> IO (U.Item [U.Peer])
getValidators = flip post' Nothing

getVersion :: Cfg.Config -> U.Path -> IO (U.Item Version.Version)
getVersion = flip post' Nothing

post' :: FromJSON a => Cfg.Config -> Maybe U.Cmd -> U.Path -> IO (U.Item a)
post' cfg mcmd p = do
  man <- newManager defaultManagerSettings
  initReq <- parseRequest (Cfg.host cfg)
  let req = if isJust mcmd then
              initReq { method = "POST" , requestBody = RequestBodyLBS (encode (fromJust mcmd))}
            else
              initReq { method = "POST", path = U.unpath p }
  res <- httpLbs req man
  print res
  return $ handleResult res

handleResult :: FromJSON a => Response BSL.ByteString -> U.Item a
handleResult res =
    if responseStatus res == status200 then
      parse . eitherDecode . responseBody $ res
    else
      Left (T.decodeUtf8 . statusMessage . responseStatus $ res)

parse :: FromJSON a => Either String RPC.RPCResponse -> U.Item a
parse (Left e)                     = Left (T.pack e)
parse (Right (RPC.RPCRespError e)) = Left (T.pack . show $ e)
parse (Right (RPC.RPCResp a))      =
  case fromJSON a of
    Success res -> Right res
    Error s     -> Left $ T.pack s
parse (Right RPC.RPCRespOK)        = Left "ok"

root :: U.Path
root = U.mkPath ""
