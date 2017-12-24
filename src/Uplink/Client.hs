{-# LANGUAGE OverloadedStrings #-}

module Uplink.Client
  ( Handle (..)

  , Account.Account (..)
--  , Asset.Asset (..)
  , AssetAddress.AssetAddress (..)
  , Block.Block (..)
  , CreateAsset (..)
  , Contract.Contract (..)
  , Config.Config (..)
  , Item
  , Path
  , Peer.Peer

  , mkAddress
  , mkPath
  , mkPathWithId
  , mkSafeString

  , unpath
  , uplinkAccounts
  , uplinkBlock
  , uplinkBlocks
  , uplinkAsset
  , uplinkAssets
  , uplinkContracts
  , uplinkPeers

  ) where

import qualified Data.ByteString as BS
import           Data.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Address
--import qualified Account
--import qualified Block
import qualified Asset
import qualified SafeString
import qualified Uplink.Client.Account as Account
import qualified Uplink.Client.AssetAddress as AssetAddress
import qualified Uplink.Client.Block as Block
import qualified Uplink.Client.Contract as Contract
import qualified Uplink.Client.Config as Config
import qualified Uplink.Client.Peer as Peer

type Item a = Either T.Text a

data Path = Path { unpath :: BS.ByteString }

mkPath :: String -> Path
mkPath = Path <$> T.encodeUtf8 . T.pack

mkPathWithId :: String -> String -> Path
mkPathWithId path identifier = Path <$> T.encodeUtf8 . T.pack $ path ++ "/" ++ identifier

data CreateAsset = CreateAsset
  { assetAddress :: Address.Address
  , assetName    :: SafeString.SafeString
  , assetQuantity :: Int64
  }

mkAddress :: IO Address.Address
mkAddress = Address.newAddr

mkSafeString :: T.Text -> SafeString.SafeString
mkSafeString = SafeString.fromBytes' . T.encodeUtf8

data Handle = Handle
  { config       :: Config.Config
  , getAccounts  :: Path -> IO (Item [Account.Account])
  , getAsset     :: Path -> IO (Item Asset.Asset)
  , getAssets    :: Path -> IO (Item [AssetAddress.AssetAddress])
  , getBlock     :: Path -> IO (Item Block.Block)
  , getBlocks    :: Path -> IO (Item [Block.Block])
  , getPeers     :: Path -> IO (Item [Peer.Peer])
  , createAsset  :: CreateAsset -> IO (Item ())
  , getContracts :: Path -> IO (Item [Contract.Contract])
  }

uplinkAccounts :: Handle -> IO (Item [Account.Account])
uplinkAccounts = (`getAccounts` mkPath "accounts")

uplinkAsset :: Handle -> String -> IO (Item Asset.Asset)
uplinkAsset h assetId =  getAsset h (mkPathWithId "/assets" assetId)

uplinkAssets :: Handle -> IO (Item [AssetAddress.AssetAddress])
uplinkAssets = (`getAssets` mkPath "assets")

uplinkBlock :: Handle -> String -> IO (Item Block.Block)
uplinkBlock h blockId = getBlock h (mkPathWithId "/blocks" blockId)

uplinkBlocks :: Handle -> IO (Item [Block.Block])
uplinkBlocks = (`getBlocks` mkPath "blocks")

uplinkContracts :: Handle  -> IO (Item [Contract.Contract])
uplinkContracts = (`getContracts` mkPath "contracts")

uplinkPeers :: Handle -> IO (Item [Peer.Peer])
uplinkPeers = (`getPeers` mkPath "peers")
