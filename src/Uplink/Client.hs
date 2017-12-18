{-# LANGUAGE OverloadedStrings #-}

module Uplink.Client
  ( Handle (..)

  , Account.Account (..)
  , Asset.Asset (..)
  , AssetAddress.AssetAddress (..)
  , Contract.Contract (..)
  , Op.Op (..)
  , Config.Config (..)
  , Item

  , uplinkAccounts
  , uplinkAssets
  , uplinkAsset
  , uplinkContracts

  ) where

import qualified Uplink.Client.Account as Account
import qualified Uplink.Client.AssetAddress as AssetAddress
import qualified Uplink.Client.Contract as Contract
import qualified Uplink.Client.Config as Config
import qualified Uplink.Client.Op as Op
import qualified Asset
import qualified Data.Text as T

type Item a = Either T.Text a

data Handle = Handle
  { getAccounts  :: String -> Config.Config -> IO (Item [Account.Account])
  , getAssets    :: String -> Config.Config -> IO (Item [AssetAddress.AssetAddress])
  , createAsset  :: Config.Config -> IO (Item ())
  , getAsset     :: String -> String -> Config.Config -> IO (Item Asset.Asset)
  , getContracts :: String -> Config.Config -> IO (Item [Contract.Contract])
  }

uplinkAssets :: Handle -> Config.Config -> IO (Item [AssetAddress.AssetAddress])
uplinkAssets = (`getAssets` "/assets")

uplinkAsset :: Handle -> String -> Config.Config -> IO (Item Asset.Asset)
uplinkAsset h = getAsset h "/assets"

uplinkContracts :: Handle -> Config.Config -> IO (Item [Contract.Contract])
uplinkContracts = (`getContracts` "/contracts")

uplinkAccounts :: Handle -> Config.Config -> IO (Item [Account.Account])
uplinkAccounts = (`getAccounts` "/accounts")
