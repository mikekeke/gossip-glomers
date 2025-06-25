{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (void)
import Data.Aeson (Object, ToJSON, encode, object, (.:), (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.TH
import Data.Aeson.Types (ToJSON (toJSON), parseMaybe)
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

echoSimpleHandler :: Maelstrom.ReqHandler
echoSimpleHandler =
    Maelstrom.simpleHandler $
        \(EchoReq inMsg) -> pure $ EchoResp inMsg

echoMessageHandler :: Maelstrom.ReqHandler
echoMessageHandler =
    Maelstrom.messageHandler $ \msg@(Maelstrom.Message _ _ body) respId -> do
        echoMessage <- case parseMaybe (.: "echo") body :: Maybe String of
            Nothing -> error $ "Failed to parse incoming message"
            Just em -> pure em
        pure $
            Maelstrom.reply msg respId $
                [ "type" .= ("echo_ok" :: String)
                , "echo" .= echoMessage
                ]

main :: IO ()
main = do
    void $
        Maelstrom.spawnNode echoMessageHandler
            >>= Maelstrom.waitNode
