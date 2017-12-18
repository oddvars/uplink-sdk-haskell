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
  { config       :: Config.Config
  , getAccounts  :: String -> IO (Item [Account.Account])
  , getAssets    :: String -> IO (Item [AssetAddress.AssetAddress])
  , createAsset  :: IO (Item ())
  , getAsset     :: String -> String -> IO (Item Asset.Asset)
  , getContracts :: String -> IO (Item [Contract.Contract])
  }

uplinkAssets :: Handle -> IO (Item [AssetAddress.AssetAddress])
uplinkAssets = (`getAssets` "/assets")

uplinkAsset :: Handle -> String -> IO (Item Asset.Asset)
uplinkAsset h = getAsset h "/assets"

uplinkContracts :: Handle -> IO (Item [Contract.Contract])
uplinkContracts = (`getContracts` "/contracts")

uplinkAccounts :: Handle -> IO (Item [Account.Account])
uplinkAccounts = (`getAccounts` "/accounts")
