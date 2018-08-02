{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Text as T

import qualified Account
import qualified Address as A
import qualified Asset
import qualified Key
import qualified Metadata
import qualified SafeString
import qualified Uplink as U

getConfig :: IO U.Config
getConfig = do
  let path = "/home/oddvar/repos/uplink/config/validators/auth0/"
  priv <- BS.readFile (path ++ "key")
  acc  <- BSL.readFile (path ++ "account")

  let account = decode acc :: Maybe Account.Account
  case Key.tryDecodePriv priv of
    (Left e)        -> fail (T.unpack e)
    (Right privKey) -> return $ U.Config privKey (origin account) "http://127.0.0.1:8545"

  where origin :: Maybe Account.Account -> A.Address A.AAccount
        origin Nothing  = A.emptyAddr -- fix
        origin (Just a) = Account.address a

main :: IO ()
main = do
  cfg <- getConfig

  contractEx cfg

accountEx :: U.Config -> IO ()
accountEx cfg =
  U.withHTTPClient cfg (\h -> U.uplinkCreateAccount h "GMT" meta) >>= print
  where meta = Metadata.Metadata (Map.fromList[("name", "Haskell")])

assetEx :: U.Config -> IO ()
assetEx cfg = do
  U.withHTTPClient cfg U.uplinkAssets >>= print
  U.withHTTPClient cfg (\h -> U.uplinkCreateAsset h "haskell asset" 100 (Just Asset.USD) Asset.Discrete meta) >>= print
  where meta = Metadata.Metadata (Map.fromList[("name", "Haskell")])

blocksEx :: U.Config -> IO ()
blocksEx cfg =
  U.withHTTPClient cfg (`U.uplinkTransactions` "1") >>= print

contractEx :: U.Config ->  IO ()
contractEx cfg = do
  putStrLn "-- create contract --"
  script <- BS.readFile "/home/oddvar/repos/uplink/examples/minimal.s"
  U.withHTTPClient cfg (`U.uplinkCreateContract` script) >>= print

  putStrLn "-- all contracts --"
  contracts <- U.withHTTPClient cfg U.uplinkContracts

  case contracts of
    Right []    -> putStrLn "no contracts found"
    Right (c:_) -> do
      let cid = show (U.contractAddress c)
      U.withHTTPClient cfg (`U.uplinkContract` cid) >>= print
      U.withHTTPClient cfg (`U.uplinkContractCallable` cid) >>= print

      let method = SafeString.fromBytes' "setX"
          addr   = A.fromRaw "JCEeU4mQ9v6HY6BKoqpdXLjmHo2N2UnAnmkQBobhxrfA"
          args   = []
      U.withHTTPClient cfg (\h -> U.uplinkCallContract h addr method args ) >>= print

    Left _     -> putStrLn "error"

infoEx :: U.Config -> IO ()
infoEx cfg = U.withHTTPClient cfg U.uplinkVersion >>= print

poolEx :: U.Config -> IO ()
poolEx cfg = do
  U.withHTTPClient cfg U.uplinkInvalidTransactions >>= print
  U.withHTTPClient cfg U.uplinkMemPool >>= print
  U.withHTTPClient cfg U.uplinkPoolSize >>= print
