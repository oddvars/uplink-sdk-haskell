{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Text as T

import qualified Asset
import qualified Account
import qualified Address
import qualified Key
import qualified Uplink as U

getConfig :: IO U.Config
getConfig = do
  let path = "/home/oddvar/repos/uplink/oddvar/"
  priv <- BS.readFile (path ++ "key")
  acc  <- BSL.readFile (path ++ "account")

  let account = decode acc :: Maybe Account.Account
  case Key.tryDecodePriv priv of
    (Left e)        -> fail (T.unpack e)
    (Right privKey) -> return $ U.Config privKey (origin account) "http://127.0.0.1:8545"

  where origin :: Maybe Account.Account -> Address.Address
        origin Nothing  = Address.parseAddress "" -- fix
        origin (Just a) = Account.address a

main :: IO ()
main = do
  cfg <- getConfig

  contractEx cfg

accountEx :: U.Config -> IO ()
accountEx cfg = do
  U.withHTTPClient cfg (\h -> U.uplinkCreateAccount h "GMT" meta) >>= print
  where meta = U.Metadata (Map.fromList[("name", "Haskell")])

assetEx :: U.Config -> IO ()
assetEx cfg = do
  U.withHTTPClient cfg U.uplinkAssets >>= print
  U.withHTTPClient cfg (\h -> U.uplinkCreateAsset h "haskell asset" 100 (Just Asset.USD) Asset.Discrete) >>= print

blocksEx :: U.Config -> IO ()
blocksEx cfg = do
  U.withHTTPClient cfg (`U.uplinkTransactions` "1") >>= print

contractEx :: U.Config ->  IO ()
contractEx cfg = do
  putStrLn "-- all contracts --"
  U.withHTTPClient cfg U.uplinkContracts >>= print

  putStrLn "-- single contracts --"
  U.withHTTPClient cfg (`U.uplinkContract` "5iPNiNwhnyYxQ1Qn496csxKVsiDs12nq1XCY5DVZeTgM") >>= print

  putStrLn "-- create contract--"
  script <- BS.readFile "/home/oddvar/repos/uplink/contracts/minimal.s"
  U.withHTTPClient cfg (`U.uplinkCreateContract` script) >>= print

transactionsEx :: U.Config -> IO ()
transactionsEx cfg = do
  U.withHTTPClient cfg U.uplinkInvalidTransactions >>= print
  U.withHTTPClient cfg U.uplinkMemPool >>= print
