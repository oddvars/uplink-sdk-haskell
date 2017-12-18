{-# LANGUAGE OverloadedStrings #-}
module Uplink.Client.Account where

import           Data.Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

data Account = Account
  { accountPublicKey :: T.Text
  , accountAddress :: T.Text
  , accountMetadata :: H.HashMap T.Text T.Text
  , accountTimeZone :: T.Text
  } deriving (Show)

instance FromJSON Account where
  parseJSON = withObject "Account" $ \v -> Account
    <$> v .: "publicKey"
    <*> v .: "address"
    <*> v .: "metadata"
    <*> v .: "timezone"
