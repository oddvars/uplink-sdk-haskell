{-# LANGUAGE OverloadedStrings #-}

module Uplink.Client
  ( Handle (..)

  , Account.Account (..)
  , Asset.Asset (..)
  , AssetAddress.AssetAddress (..)
  , CreateAsset (..)
  , Contract.Contract (..)
  , Op.Op (..)
  , Config.Config (..)
  , Item
  , Path

  , mkAddress
  , mkPath
  , mkPathWithId
  , mkSafeString

  , unpath
  , uplinkAccounts
  , uplinkAssets
  , uplinkAsset
  , uplinkContracts

  ) where

import qualified Address
import qualified Asset
import qualified Data.ByteString as BS
import           Data.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified SafeString

import qualified Uplink.Client.Account as Account
import qualified Uplink.Client.AssetAddress as AssetAddress
import qualified Uplink.Client.Contract as Contract
import qualified Uplink.Client.Config as Config
import qualified Uplink.Client.Op as Op

type Item a = Either T.Text a

data Path = Path { unpath :: BS.ByteString }

mkPath :: String -> Path
mkPath = Path <$> T.encodeUtf8 . T.pack

mkPathWithId :: String -> String -> Path
mkPathWithId p _id = Path <$> T.encodeUtf8 . T.pack $ p ++ "/" ++ _id


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
  , getAssets    :: Path -> IO (Item [AssetAddress.AssetAddress])
  , createAsset  :: CreateAsset -> IO (Item ())
  , getAsset     :: Path -> IO (Item Asset.Asset)
  , getContracts :: Path -> IO (Item [Contract.Contract])
  }

uplinkAssets :: Handle -> IO (Item [AssetAddress.AssetAddress])
uplinkAssets = (`getAssets` mkPath "assets")

uplinkAsset :: Handle -> String -> IO (Item Asset.Asset)
uplinkAsset h assetId =  getAsset h (mkPathWithId "/assets" assetId)

uplinkContracts :: Handle  -> IO (Item [Contract.Contract])
uplinkContracts = (`getContracts` mkPath "contracts")

uplinkAccounts :: Handle -> IO (Item [Account.Account])
uplinkAccounts = (`getAccounts` mkPath "accounts")
