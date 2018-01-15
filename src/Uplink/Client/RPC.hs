{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module Uplink.Client.RPC where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text
import           GHC.Generics

import qualified Script.Parser as Parser

data RPCResponse
  = RPCResp { contents :: Value}
  | RPCRespError RPCResponseError
  | RPCRespOK
  deriving (Show, Generic, FromJSON)

data RPCResponseError
  = InvalidParam Text
  | Internal Text
  | ContractParse Parser.ParseErrInfo
  | ContractType Text
  | ContractGet Text
  | JSONParse Text
  | ReadOnly
  | Tx Text
  | NotTestNode
  | NotFound Text
  deriving (Eq, Show, Generic)

instance FromJSON RPCResponseError where
  parseJSON (Object o) = do
    errorType <- o .: "errorType"
    case errorType :: Text of
      "InvalidParam"  -> InvalidParam <$> o .: "errorMsg"
      "Internal"      -> Internal <$> o .: "errorMsg"
      "ContractParse" -> ContractParse <$> o .: "errorMsg"
      "ContractType"  -> ContractType <$> o .: "errorMsg"
      "ContractGet"   -> ContractGet <$> o .: "errorMsg"
      "JSONParse"     -> JSONParse <$> o .: "errorMsg"
      "ReadOnly"      -> pure ReadOnly
      "Tx"            -> Tx <$> o .: "errorMsg"
      "NotTestNode"   -> pure NotTestNode
      "NotFound "     -> NotFound <$> o .: "errorMsg"
  parseJSON invalid = typeMismatch "RPCResponseError" invalid
