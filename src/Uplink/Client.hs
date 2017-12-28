{-# LANGUAGE OverloadedStrings #-}

module Uplink.Client
  ( Handle (..)

  , Account.Account (..)
  , Account.Metadata (..)
  , AssetAddress.AssetAddress (..)
  , Block.Block (..)
  , Cmd (..)
  , Contract.Contract (..)
  , Config.Config (..)
  , Item
  , Path
  , Peer.Peer
  , Transaction.Transaction (..)
  , Tx.TxAccount (..)
  , Tx.TxAsset (..)

  , mkAddress
  , mkPath
  , mkPathWithId
  , mkSafeString

  , pubToBS

  , unpath
  , uplinkAccount
  , uplinkAccounts
  , uplinkCreateAccount
  , uplinkBlock
  , uplinkBlocks
  , uplinkAsset
  , uplinkAssets
  , uplinkCreateAsset
  , uplinkContract
  , uplinkContracts
  , uplinkPeers
  , uplinkTransactions
  , uplinkValidators
--  , uplinkVersion

  ) where

import           Data.Aeson
import qualified Data.ByteString as BS
import           Data.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Serialize as S

import qualified Address
import qualified Account
import qualified Asset
import qualified Derivation as D
import qualified Key
import qualified SafeString
import qualified Time
import qualified Transaction as Tx
import qualified Uplink.Client.AssetAddress as AssetAddress
import qualified Uplink.Client.Block as Block
import qualified Uplink.Client.Contract as Contract
import qualified Uplink.Client.Config as Config
import qualified Uplink.Client.Peer as Peer
import qualified Uplink.Client.Version as Version
import qualified Uplink.Client.Transaction as Transaction

type Item a = Either T.Text a

data Path = Path { unpath :: BS.ByteString }

data Cmd = Cmd
  { params :: Tx.Transaction
  , method' :: T.Text
  } deriving (Show)

instance ToJSON Cmd where
  toJSON cmd = object [ "params" .= toJSON (params cmd)
                      , "method" .= method' cmd
                      ]



mkPath :: String -> Path
mkPath = Path <$> T.encodeUtf8 . T.pack

mkPathWithId :: String -> String -> Path
mkPathWithId path identifier = Path <$> T.encodeUtf8 . T.pack $ path ++ "/" ++ identifier

mkAddress :: IO Address.Address
mkAddress = Address.newAddr

mkSafeString :: String -> SafeString.SafeString
mkSafeString = SafeString.fromBytes' . T.encodeUtf8 . T.pack

data Handle = Handle
  { config          :: Config.Config
  , createAsset     :: Maybe Cmd -> IO (Item ())
  , createAccount   :: Maybe Cmd -> IO (Item ())
  , getAccount      :: Path -> IO (Item Account.Account)
  , getAccounts     :: Path -> IO (Item [Account.Account])
  , getAsset        :: Path -> IO (Item Asset.Asset)
  , getAssets       :: Path -> IO (Item [AssetAddress.AssetAddress])
  , getBlock        :: Path -> IO (Item Block.Block)
  , getBlocks       :: Path -> IO (Item [Block.Block])
  , getPeers        :: Path -> IO (Item [Peer.Peer])
  , getValidators   :: Path -> IO (Item [Peer.Peer])
  , getContract     :: Path -> IO (Item Contract.Contract)
  , getContracts    :: Path -> IO (Item [Contract.Contract])
  , getTransactions :: Path -> IO (Item [Transaction.Transaction])
  , getVersion      :: Path -> IO (Item Version.Version)
  }

uplinkAccount :: Handle -> String -> IO (Item Account.Account)
uplinkAccount h accountId = getAccount h $ mkPathWithId "/accounts" accountId

uplinkAccounts :: Handle -> IO (Item [Account.Account])
uplinkAccounts = (`getAccounts` mkPath "accounts")

uplinkCreateAccount
  :: Handle
  -> BS.ByteString     -- timezone
  -> Account.Metadata
  -> IO (Item ())
uplinkCreateAccount h tz md = do
  (priv, pub, addr) <- Address.newTriple
  ts <- Time.now
  sig <- Key.sign priv $ S.encode (header pub)

  createAccount h (pure (mkTrans (header pub) sig addr ts))

  where
    header :: Key.PubKey -> Tx.TransactionHeader
    header pubKey = Tx.TxAccount Tx.CreateAccount { Tx.pubKey = pubToBS pubKey
                                                  , Tx.timezone = tz
                                                  , Tx.metadata = md }

mkTrans :: Tx.TransactionHeader -> Key.Signature -> Address.Address -> Time.Timestamp -> Cmd
mkTrans hdr sig addr' ts' = Cmd (Tx.Transaction hdr (Key.encodeSig sig) addr' ts') "Transaction"

uplinkCreateAsset
  :: Handle
  -> BS.ByteString    -- name
  -> Int64            -- supply
  -> Maybe Asset.Ref
  -> Asset.AssetType
  -> IO (Item ())
uplinkCreateAsset h n supply mRef atyp = do
  ts <- Time.now
  let cfg     = config h
      origin  = Config.originAddress cfg
      da      = D.addrAsset n origin supply mRef atyp ts
      name'   = SafeString.fromBytes' n
      txAsset = Tx.CreateAsset da name' supply mRef atyp
      header' = Tx.TxAsset txAsset

  sig <- Key.sign (Config.privateKey cfg) $ S.encode header'

  createAsset h (pure (mkTrans header' sig origin ts))

uplinkAsset :: Handle -> String -> IO (Item Asset.Asset)
uplinkAsset h assetId = getAsset h $ mkPathWithId "/assets" assetId

uplinkAssets :: Handle -> IO (Item [AssetAddress.AssetAddress])
uplinkAssets = (`getAssets` mkPath "assets")

uplinkBlock :: Handle -> String -> IO (Item Block.Block)
uplinkBlock h blockId = getBlock h (mkPathWithId "/blocks" blockId)

uplinkBlocks :: Handle -> IO (Item [Block.Block])
uplinkBlocks = (`getBlocks` mkPath "blocks")

uplinkContract :: Handle -> String -> IO (Item Contract.Contract)
uplinkContract h contractId = getContract h $ mkPathWithId "/contracts" contractId

uplinkContracts :: Handle  -> IO (Item [Contract.Contract])
uplinkContracts = (`getContracts` mkPath "contracts")

uplinkPeers :: Handle -> IO (Item [Peer.Peer])
uplinkPeers = (`getPeers` mkPath "peers")

uplinkValidators :: Handle -> IO (Item [Peer.Peer])
uplinkValidators = (`getValidators` mkPath "peers/validators")

uplinkTransactions :: Handle -> String -> IO (Item [Transaction.Transaction])
uplinkTransactions h blockId = getTransactions h $ mkPathWithId "/transactions" blockId

pubToBS :: Key.PubKey -> BS.ByteString
pubToBS = Key.unHexPub . Key.hexPub
