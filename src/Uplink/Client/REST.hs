{-# LANGUAGE OverloadedStrings #-}
module Uplink.Client.REST
  ( withHandle
  )
  where

import           Data.Aeson
import           Data.Maybe
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Client
import           Network.HTTP.Types


import qualified Asset
import qualified Key
import qualified RPC
import qualified Time
import qualified Uplink.Client as U
import qualified Uplink.Client.Config as Cfg
import qualified Transaction as Tx

data Cmd = Cmd
  { params :: Tx.Transaction
  , method' :: T.Text
  } deriving (Show)

instance ToJSON Cmd where
  toJSON cmd = object [ "params" .= toJSON (params cmd)
                      , "method" .= method' cmd
                      ]

withHandle :: Cfg.Config -> (U.Handle -> IO (U.Item a)) -> IO (U.Item a)
withHandle cfg f = f U.Handle
  { U.config       = cfg
  , U.getAccounts  = loadAccounts cfg
  , U.getAssets    = loadAssets cfg
  , U.getAsset     = loadAsset cfg
  , U.getContracts = loadContracts cfg
  , U.createAsset  = createAsset cfg
  }

loadAccounts :: Cfg.Config -> U.Path -> IO (U.Item [U.Account])
loadAccounts cfg = post' cfg Nothing

createAsset :: Cfg.Config -> U.CreateAsset -> IO (U.Item ())
createAsset cfg@(Cfg.Config priv orig _) (U.CreateAsset addr name qty) = do
  let header = Tx.TxAsset (Tx.CreateAsset addr name qty (Just Asset.USD) Asset.Discrete)

  ts <- Time.now
  sig <- Key.sign priv $ S.encode header

  post' cfg (Just (Cmd (Tx.Transaction header (Key.encodeSig sig) orig ts) "Transaction")) (U.mkPath "")

loadAssets :: Cfg.Config -> U.Path -> IO (U.Item [U.AssetAddress])
loadAssets cfg = post' cfg Nothing

loadAsset :: Cfg.Config -> U.Path -> IO (U.Item U.Asset)
loadAsset cfg = post' cfg Nothing

loadContracts :: Cfg.Config -> U.Path -> IO (U.Item [U.Contract])
loadContracts cfg = post' cfg Nothing

post' :: FromJSON a => Cfg.Config -> Maybe Cmd -> U.Path -> IO (U.Item a)
post' cfg mcmd p = do
  man <- newManager defaultManagerSettings
  initReq <- parseRequest (Cfg.host cfg)
  let req = if isJust mcmd then
              initReq { method = "POST" , requestBody = RequestBodyLBS (encode (fromJust mcmd))}
            else
              initReq { method = "POST", path = U.unpath p }
  res <- httpLbs req man
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
