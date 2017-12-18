{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Account
import qualified Address
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe (maybe)
import qualified Key
import qualified Uplink

main :: IO ()
main = do
  let path = "/home/oddvar/repos/uplink/oddvar/"

  priv <- BS.readFile (path ++ "key")
  acc  <- BSL.readFile (path ++ "account")

  let account = decode acc :: Maybe Account.Account

  print account
  case Key.tryDecodePriv priv of
    (Left e)        -> print e
    (Right privKey) -> do
      let cfg = Uplink.Config privKey (origin account) "http://127.0.0.1:8545"

      --let assetId = "J1qbWXdZ4nAVZuch5tSUeoXuX8j37vP98aum4RcPzfvN"
      --Uplink.withHandle (`Uplink.uplinkAsset` assetId) >>= print

      Uplink.withHandle cfg (`Uplink.uplinkAssets` cfg) >>= print
      Uplink.withHandle cfg (`Uplink.createAsset` cfg)  >>= print

  where origin :: Maybe Account.Account -> Address.Address
        origin Nothing =   Address.parseAddress ""
        origin (Just a)  = Account.address a
