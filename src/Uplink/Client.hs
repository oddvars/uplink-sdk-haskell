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

  , mkAddress
  , mkSafeString

  , uplinkAccounts
  , uplinkAssets
  , uplinkAsset
  , uplinkContracts

  ) where

import qualified Address
import qualified Asset
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
  , getAccounts  :: String -> IO (Item [Account.Account])
  , getAssets    :: String -> IO (Item [AssetAddress.AssetAddress])
  , createAsset  :: CreateAsset -> IO (Item ())
  , getAsset     :: String -> String -> IO (Item Asset.Asset)
  , getContracts :: String -> IO (Item [Contract.Contract])
  }

uplinkAssets :: Handle  -> IO (Item [AssetAddress.AssetAddress])
uplinkAssets = (`getAssets` "assets")

uplinkAsset :: Handle  -> String -> IO (Item Asset.Asset)
uplinkAsset = (`getAsset` "/assets")

uplinkContracts :: Handle  -> IO (Item [Contract.Contract])
uplinkContracts = (`getContracts` "contracts")

uplinkAccounts :: Handle -> IO (Item [Account.Account])
uplinkAccounts = (`getAccounts` "accounts")
