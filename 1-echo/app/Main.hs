{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Aeson (ToJSON, object, (.=))
import Data.Aeson.TH
import Data.Aeson.Types (ToJSON (toJSON))
import Node qualified

data EchoReq = EchoReq
  { echo :: String
  }

$(deriveJSON defaultOptions ''EchoReq)

data EchoResp = EchoResp String

instance ToJSON EchoResp where
  toJSON (EchoResp e) =
    object
      [ "type" .= ("echo_ok" :: String)
      , "echo" .= e
      ]

nodeHandler :: EchoReq -> EchoResp
nodeHandler (EchoReq e) = EchoResp e

main :: IO ()
main = do
  _n <- Node.spawnNode nodeHandler
  pure ()