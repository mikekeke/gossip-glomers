{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (void)
import Data.Aeson (ToJSON, object, (.=))
import Data.Aeson.TH
import Data.Aeson.Types (ToJSON (toJSON))
import Maelstrom qualified

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

echoHandler :: EchoReq -> IO EchoResp
echoHandler (EchoReq e) = pure $ EchoResp e

main :: IO ()
main = do
    void $
        Maelstrom.spawnNode (Maelstrom.simpleHandler echoHandler)
            >>= Maelstrom.waitNode
