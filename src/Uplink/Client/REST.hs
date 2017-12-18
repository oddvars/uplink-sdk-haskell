{-# LANGUAGE OverloadedStrings #-}
module Uplink.Client.REST
  ( withHandle
  )
  where
import           Data.Aeson
import qualified Address
import qualified Asset
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Client
import           Network.HTTP.Types
import qualified RPC
import qualified Time
import qualified Uplink.Client as Uplink
import qualified Uplink.Client.Config as Config
import qualified Transaction as Tx
import qualified SafeString
import qualified Key

data Cmd = Cmd
  { params :: Tx.Transaction
  , method' :: T.Text
  } deriving (Show)

instance ToJSON Cmd where
  toJSON cmd = object [ "params" .= toJSON (params cmd)
                      , "method" .= method' cmd
                      ]

withHandle :: Config.Config -> (Uplink.Handle -> IO (Uplink.Item a)) -> IO (Uplink.Item a)
withHandle cfg f = f Uplink.Handle
  { Uplink.config       = cfg
  , Uplink.getAccounts  = loadAccounts cfg
  , Uplink.getAssets    = loadAssets cfg
  , Uplink.getAsset     = loadAsset cfg
  , Uplink.getContracts = loadContracts cfg
  , Uplink.createAsset  = createAsset cfg
  }

createAsset :: Config.Config -> IO (Uplink.Item ())
createAsset cfg = do
  ts <- Time.now
  address <- Address.newAddr
  let origin = Config.originAddress cfg
      name   = SafeString.fromBytes' ("create asset from haskell" :: BS.ByteString)
      header = Tx.TxAsset (Tx.CreateAsset address name 1 (Just Asset.USD) Asset.Discrete)

  sig <- Key.sign (Config.privateKey cfg) $ S.encode header

  let
      transFailing = Tx.Transaction header "" origin ts
      transOk = Tx.Transaction header (Key.encodeSig sig) origin ts
      rpc   = Cmd transFailing "Transaction"

  print rpc
  push cfg rpc

loadAccounts :: Config.Config -> String -> IO (Uplink.Item [Uplink.Account])
loadAccounts cfg url = fetch cfg url ""

loadAssets :: Config.Config -> String -> IO (Uplink.Item [Uplink.AssetAddress])
loadAssets url = fetch url ""

loadAsset :: Config.Config -> String -> String -> IO (Uplink.Item Uplink.Asset)
loadAsset = fetch

loadContracts :: Config.Config -> String -> IO (Uplink.Item [Uplink.Contract])
loadContracts url = fetch url ""

fetch :: FromJSON a =>  Config.Config -> String -> String -> IO (Uplink.Item a)
fetch cfg s _id = do
  man <- newManager defaultManagerSettings
  initReq <- parseRequest $ "http://127.0.0.1:8545" ++ s ++ "/" ++ _id
  let req = initReq { method = "POST" }
  res <- httpLbs req man
  return $
    if responseStatus res == status200 then
      parse . eitherDecode . responseBody $ res
    else
      Left (T.decodeUtf8 . statusMessage . responseStatus $ res) :: Uplink.Item a


push :: FromJSON a => Config.Config -> Cmd -> IO (Uplink.Item a)
push cfg cmd = do
  man <- newManager defaultManagerSettings
  initReq <- parseRequest (Config.host cfg)
  let req = initReq { method = "POST"
                    , requestBody = RequestBodyLBS (encode cmd)
                    }
  res <- httpLbs req man
  --print res
  return $
    if responseStatus res == status200 then
      parse . eitherDecode . responseBody $ res
    else
      Left (T.decodeUtf8 . statusMessage . responseStatus $ res) :: Uplink.Item a



parse :: FromJSON a => Either String RPC.RPCResponse -> Uplink.Item a
parse (Left e)                     = Left (T.pack e)
parse (Right (RPC.RPCRespError e)) = Left (T.pack . show $ e)
parse (Right (RPC.RPCResp a))      = mapLeft T.pack (eitherDecode (encode a))
parse (Right a)                    = mapLeft T.pack (eitherDecode (encode a))

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a)  = Left (f a)
mapLeft _ (Right b) = Right b
