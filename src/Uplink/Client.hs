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
  , Mempool.Mempool (..)
  , Mempool.Size (..)
  , Path
  , Peer.Peer
  , Contract.Method
  , Tx.Transaction (..)
  , Tx.InvalidTransaction (..)
  , Tx.TxAccount (..)
  , Tx.TxAsset (..)

  , mkAddress
  , mkPath
  , mkPathWithId
  , mkPathWithIdAndRoute
  , mkSafeString

  , pubToBS

  , unpath
  , uplinkAccount
  , uplinkAccounts
  , uplinkBlock
  , uplinkBlocks
  , uplinkAsset
  , uplinkAssets
  , uplinkCallContract
  , uplinkCirculateAsset
  , uplinkCreateAccount
  , uplinkCreateAsset
  , uplinkCreateContract
  , uplinkContract
  , uplinkContractCallable
  , uplinkContracts
  , uplinkInvalidTransactions
  , uplinkMemPool
  , uplinkMemPools
  , uplinkPeers
  , uplinkPoolSize
  , uplinkPoolSizes
  , uplinkRevokeAccount
  , uplinkRevokeAsset
  , uplinkTransactions
  , uplinkTransferAsset
  , uplinkValidators
  , uplinkVersion

  ) where

import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Serialize as S

import qualified Address
import qualified Account
import qualified Asset
import qualified Derivation as D
import qualified Encoding
import qualified Key
--import qualified RPC
import qualified SafeString
import qualified Storage
import qualified Time
import qualified Uplink.Client.AssetAddress as AssetAddress
import qualified Uplink.Client.Block as Block
import qualified Uplink.Client.Contract as Contract
import qualified Uplink.Client.Config as Config
import qualified Uplink.Client.Mempool as Mempool
import qualified Uplink.Client.Peer as Peer
import qualified Uplink.Client.RPC as RPC
import qualified Uplink.Client.Version as Version
import qualified Uplink.Client.Transaction as Tx

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

data Handle = Handle
  { config                 :: Config.Config

  -- executes
  , callContract           :: Cmd  -> IO (Item RPC.RPCResponse)
  , circulateAsset         :: Cmd  -> IO (Item RPC.RPCResponse)
  , createAsset            :: Cmd  -> IO (Item RPC.RPCResponse)
  , createAccount          :: Cmd  -> IO (Item RPC.RPCResponse)
  , createContract         :: Cmd  -> IO (Item RPC.RPCResponse)
  , revokeAccount          :: Cmd  -> IO (Item RPC.RPCResponse)
  , revokeAsset            :: Cmd  -> IO (Item RPC.RPCResponse)
  , transferAsset          :: Cmd  -> IO (Item RPC.RPCResponse)

  -- views
  , getAccount             :: Path -> IO (Item Account.Account)
  , getAccounts            :: Path -> IO (Item [Account.Account])
  , getAsset               :: Path -> IO (Item Asset.Asset)
  , getAssets              :: Path -> IO (Item [AssetAddress.AssetAddress])
  , getBlock               :: Path -> IO (Item Block.Block)
  , getBlocks              :: Path -> IO (Item [Block.Block])
  , getContract            :: Path -> IO (Item Contract.Contract)
  , getContractCallable    :: Path -> IO (Item Contract.Method)
  , getContracts           :: Path -> IO (Item [Contract.Contract])
  , getInvalidTransactions :: Path -> IO (Item [Tx.InvalidTransaction])
  , getMempool             :: Path -> IO (Item Mempool.Mempool)
  , getMempools            :: Path -> IO (Item Value) -- todo: set real type
  , getPeers               :: Path -> IO (Item [Peer.Peer])
  , getPoolSize            :: Path -> IO (Item Mempool.Size)
  , getPoolSizes           :: Path -> IO (Item Value) -- todo: set real type
  , getTransactions        :: Path -> IO (Item [Tx.Transaction])
  , getValidators          :: Path -> IO (Item [Peer.Peer])
  , getVersion             :: Path -> IO (Item Version.Version)
  }

-- execute functions
uplinkCallContract
  :: Handle
  -> Address.Address
  -> SafeString.SafeString -- method
  -> [Storage.Value] -- args
  -> IO (Item RPC.RPCResponse)
uplinkCallContract h a m args = do
  ts <- Time.now
  let cfg = config h
      header = Tx.TxContract $ Tx.Call a m args
      origin = Config.originAddress cfg
  sig <- Key.sign (Config.privateKey cfg) $ S.encode header

  h `callContract` mkTrans header sig origin ts

uplinkCirculateAsset
  :: Handle
  -> Address.Address -- address of asset
  -> Int64
  -> IO (Item RPC.RPCResponse)
uplinkCirculateAsset h t a = do
  ts <- Time.now
  let cfg = config h
      header = Tx.TxAsset $ Tx.Circulate t a
      origin = Config.originAddress cfg
  sig <- Key.sign (Config.privateKey cfg) $ S.encode header
  h `circulateAsset` mkTrans header sig origin ts

uplinkCreateAccount
  :: Handle
  -> BS.ByteString     -- timezone
  -> Account.Metadata
  -> IO (Item RPC.RPCResponse)
uplinkCreateAccount h tz md = do
  (priv, pub, addr) <- Address.newTriple
  ts <- Time.now
  sig <- Key.sign priv $ S.encode (header pub)

  h `createAccount `mkTrans (header pub) sig addr ts

  where
    header :: Key.PubKey -> Tx.TransactionHeader
    header pubKey = Tx.TxAccount Tx.CreateAccount { Tx.pubKey = pubToBS pubKey
                                                  , Tx.timezone = tz
                                                  , Tx.metadata = md }

uplinkCreateAsset
  :: Handle
  -> BS.ByteString    -- name
  -> Int64            -- supply
  -> Maybe Asset.Ref
  -> Asset.AssetType
  -> IO (Item RPC.RPCResponse)
uplinkCreateAsset h n supply mRef atyp = do
  ts <- Time.now
  let cfg     = config h
      origin  = Config.originAddress cfg
      da      = D.addrAsset n origin supply mRef atyp ts
      name'   = SafeString.fromBytes' n
      txAsset = Tx.CreateAsset da name' supply mRef atyp
      header = Tx.TxAsset txAsset

  sig <- Key.sign (Config.privateKey cfg) $ S.encode header

  h `createAsset` mkTrans header sig origin ts

uplinkCreateContract
  :: Handle
  -> BS.ByteString -- script
  -> IO (Item RPC.RPCResponse)
uplinkCreateContract h script = do
  ts <- Time.now
  let s          = SafeString.fromBytes' script
      cfg        = config h
      origin     = Config.originAddress cfg
      a          = derive ts script
      txContract = Tx.CreateContract a s ts origin
      header    = Tx.TxContract txContract

  sig <- Key.sign (Config.privateKey cfg) $ S.encode header

  h `createContract` mkTrans header sig origin ts
  where
    derive :: Time.Timestamp -> BS.ByteString -> Address.Address
    derive ts scr = Address.fromRaw (Encoding.b58hash (BS.concat [ BSC.pack (show ts), scr]))


uplinkRevokeAccount
  :: Handle
  -> BS.ByteString -- address to revoke
  -> IO (Item RPC.RPCResponse)
uplinkRevokeAccount h addr = do
  let cfg = config h
  ts <- Time.now
  sig <- Key.sign (Config.privateKey cfg) $ S.encode header

  h `revokeAccount` mkTrans header sig (Config.originAddress cfg) ts

  where
    header :: Tx.TransactionHeader
    header = Tx.TxAccount $ Tx.RevokeAccount (Address.fromRaw addr)

uplinkRevokeAsset
  :: Handle
  -> Address.Address -- address to revoke
  -> IO (Item RPC.RPCResponse)
uplinkRevokeAsset h addr = do
  let cfg = config h
  ts <- Time.now
  sig <- Key.sign (Config.privateKey cfg) $ S.encode header

  h `revokeAsset` mkTrans header sig (Config.originAddress cfg) ts

  where
    header :: Tx.TransactionHeader
    header = Tx.TxAsset $ Tx.RevokeAsset addr

uplinkTransferAsset
  :: Handle
  -> Address.Address  -- asset address
  -> Address.Address  -- to
  -> Int64
  -> IO (Item RPC.RPCResponse)
uplinkTransferAsset h a t b  = do
  ts <- Time.now
  let cfg = config h
      header = Tx.TxAsset $ Tx.Transfer a t b
      origin     = Config.originAddress cfg
  sig <- Key.sign (Config.privateKey cfg) $ S.encode header

  h `transferAsset` mkTrans header sig origin ts


-- view functions
uplinkAccount :: Handle -> String -> IO (Item Account.Account)
uplinkAccount h accountId = getAccount h $ mkPathWithId "/accounts" accountId

uplinkAccounts :: Handle -> IO (Item [Account.Account])
uplinkAccounts = (`getAccounts` mkPath "accounts")

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

uplinkContractCallable :: Handle -> String -> IO (Item Contract.Method)
uplinkContractCallable h contractId = getContractCallable h $ mkPathWithIdAndRoute "/contracts" contractId "callable"

uplinkContracts :: Handle  -> IO (Item [Contract.Contract])
uplinkContracts = (`getContracts` mkPath "contracts")

uplinkInvalidTransactions :: Handle -> IO (Item [Tx.InvalidTransaction])
uplinkInvalidTransactions = (`getInvalidTransactions` mkPath "/transactions/invalid")

uplinkMemPool :: Handle -> IO (Item Mempool.Mempool)
uplinkMemPool = (`getMempool` mkPath "/transactions/pool")

uplinkMemPools :: Handle -> IO (Item Value)
uplinkMemPools = (`getMempools` mkPath "/transactions/pool/all")

uplinkPeers :: Handle -> IO (Item [Peer.Peer])
uplinkPeers = (`getPeers` mkPath "peers")

uplinkValidators :: Handle -> IO (Item [Peer.Peer])
uplinkValidators = (`getValidators` mkPath "peers/validators")

uplinkPoolSize :: Handle -> IO (Item Mempool.Size)
uplinkPoolSize = (`getPoolSize` mkPath "transactions/pool/size")

uplinkPoolSizes :: Handle -> IO (Item Value)
uplinkPoolSizes = (`getPoolSizes` mkPath "transactions/pool/all/sizes")

uplinkVersion :: Handle -> IO (Item Version.Version)
uplinkVersion = (`getVersion` mkPath "/version")

uplinkTransactions :: Handle -> String -> IO (Item [Tx.Transaction])
uplinkTransactions h blockId = getTransactions h $ mkPathWithId "/transactions" blockId

-- helpers
mkTrans :: Tx.TransactionHeader -> Key.Signature -> Address.Address -> Time.Timestamp -> Cmd
mkTrans hdr sig addr' ts' = Cmd (Tx.Transaction hdr (Key.encodeSig sig) addr' ts') "Transaction"

pubToBS :: Key.PubKey -> BS.ByteString
pubToBS = Key.unHexPub . Key.hexPub

mkPath :: String -> Path
mkPath = Path <$> T.encodeUtf8 . T.pack

mkPathWithId :: String -> String -> Path
mkPathWithId path identifier = Path <$> T.encodeUtf8 . T.pack $ path ++ "/" ++ identifier

mkPathWithIdAndRoute :: String -> String -> String -> Path
mkPathWithIdAndRoute path identifier route = Path <$> T.encodeUtf8 . T.pack $ path ++ "/" ++ identifier ++ "/" ++ route

mkAddress :: IO Address.Address
mkAddress = Address.newAddr

mkSafeString :: String -> SafeString.SafeString
mkSafeString = SafeString.fromBytes' . T.encodeUtf8 . T.pack
