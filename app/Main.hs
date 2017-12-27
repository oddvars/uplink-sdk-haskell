{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Asset
import qualified Account
import qualified Address
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
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

      accountEx cfg
      --contractEx cfg
      assetEx cfg

  where origin :: Maybe Account.Account -> Address.Address
        origin Nothing  = Address.parseAddress "" -- fix
        origin (Just a) = Account.address a

accountEx :: U.Config -> IO ()
accountEx cfg = do
  U.withHTTPClient cfg (\h -> U.uplinkCreateAccount h "GMT" meta) >>= print
  where meta = U.Metadata (Map.fromList[("name", "Haskell")])

assetEx :: U.Config -> IO ()
assetEx cfg = do
  newAddr <- U.mkAddress
  --U.withHTTPClient cfg U.uplinkAssets >>= print
  U.withHTTPClient cfg (`U.uplinkCreateAsset` U.CreateAsset newAddr (U.mkSafeString "haskell asset") 100 (Just Asset.USD) Asset.Discrete) >>= print

blocksEx :: U.Config -> IO ()
blocksEx cfg = do
  U.withHTTPClient cfg (`U.uplinkTransactions` "1") >>= print

contractEx :: U.Config ->  IO ()
contractEx cfg = do
  U.withHTTPClient cfg U.uplinkContracts >>= print
  U.withHTTPClient cfg (`U.uplinkContract` "5iPNiNwhnyYxQ1Qn496csxKVsiDs12nq1XCY5DVZeTgM") >>= print
