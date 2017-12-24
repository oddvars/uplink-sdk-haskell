{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Account
import qualified Address
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Key
import qualified Uplink as U

main :: IO ()
main = do
  let path = "/home/oddvar/repos/uplink/oddvar/"

  priv <- BS.readFile (path ++ "key")
  acc  <- BSL.readFile (path ++ "account")

  let account = decode acc :: Maybe Account.Account

  case Key.tryDecodePriv priv of
    (Left e)        -> print e
    (Right privKey) -> do
      let cfg = U.Config privKey (origin account) "http://127.0.0.1:8545"

      runExample cfg

  where origin :: Maybe Account.Account -> Address.Address
        origin Nothing =   Address.parseAddress ""
        origin (Just a)  = Account.address a


runExample :: U.Config ->  IO ()
runExample cfg = do
  --newAddr <- U.mkAddress
  --let ca      = U.CreateAsset newAddr (U.mkSafeString "name") 100
  --    assetId = "J1qbWXdZ4nAVZuch5tSUeoXuX8j37vP98aum4RcPzfvN"

  --U.withHandle cfg (`U.uplinkAsset` assetId) >>= print

  U.withHandle cfg U.uplinkValidators >>= print
