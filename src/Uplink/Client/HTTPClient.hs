{-# LANGUAGE OverloadedStrings #-}
module Uplink.Client.HTTPClient
  ( withHTTPClient
  )
  where

import           Data.Aeson
import           Data.Maybe
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Client
import           Network.HTTP.Types


import qualified Asset
import qualified Derivation as D
import qualified Key
import qualified RPC
import qualified Time
import qualified Uplink.Client as U
import qualified Uplink.Client.Config as Cfg
import qualified Uplink.Client.Version as Version
import qualified Transaction as Tx
import qualified SafeString

withHTTPClient :: Cfg.Config -> (U.Handle -> IO (U.Item a)) -> IO (U.Item a)
withHTTPClient cfg f = f U.Handle
  { U.config          = cfg
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
  , U.getPeers        = getPeers cfg
  , U.getTransactions = getTransactions cfg
  , U.getValidators   = getValidators cfg
  , U.getVersion      = getVersion cfg
  }

getAccount :: Cfg.Config -> U.Path -> IO (U.Item U.Account)
getAccount = flip post' Nothing

getAccounts :: Cfg.Config -> U.Path -> IO (U.Item [U.Account])
getAccounts = flip post' Nothing

createAccount :: Cfg.Config -> Maybe U.Cmd -> IO (U.Item ())
createAccount cfg createAcct = do
  -- let header = Tx.TxAccount createAcct

  -- ts <- Time.now
  -- sig <- Key.sign (Cfg.privateKey cfg) $ S.encode header

  -- print (Key.decodeSig (Key.encodeSig sig))

  --post' cfg (Just (Cmd (Tx.Transaction header (Key.encodeSig sig) (Cfg.originAddress cfg) ts) "Transaction")) root
  post' cfg createAcct root

createAsset :: Cfg.Config -> Tx.TxAsset-> IO (U.Item ())
createAsset cfg txAsset@(Tx.CreateAsset _ name supply mRef atyp) = do
  ts <- Time.now

  let derivedAddress = D.addrAsset (SafeString.toBytes name) (Cfg.originAddress cfg) supply mRef atyp ts
      header = Tx.TxAsset txAsset { Tx.assetAddr = derivedAddress }

  sig <- Key.sign (U.privateKey cfg) $ S.encode header

  post' cfg (Just (U.Cmd (Tx.Transaction header (Key.encodeSig sig) (U.originAddress cfg) ts) "Transaction")) root

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

getPeers :: Cfg.Config -> U.Path -> IO (U.Item [U.Peer])
getPeers = flip post' Nothing

getTransactions :: Cfg.Config -> U.Path -> IO (U.Item [U.Transaction])
getTransactions = flip post' Nothing

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
      Left (T.decodeUtf8 . statusMessage . responseStatus $ res) :: U.Item a


parse :: FromJSON a => Either String RPC.RPCResponse -> U.Item a
parse (Left e)                     = Left (T.pack e)
parse (Right (RPC.RPCRespError e)) = Left (T.pack . show $ e)
parse (Right (RPC.RPCResp a))      = mapLeft T.pack (eitherDecode (encode a))
parse (Right a)                    = mapLeft T.pack (eitherDecode (encode a))

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a)  = Left (f a)
mapLeft _ (Right b) = Right b

root :: U.Path
root = U.mkPath ""
