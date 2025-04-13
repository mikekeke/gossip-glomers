module Node (
    spawnNode,
    ReqHandler,
) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON, Value (String), eitherDecode, eitherDecode', encode)
import Data.ByteString.Lazy.Char8 qualified as C8
import Data.Text (Text)
import Data.Void (Void)
import GHC.Exts (fromString)
import Message (InitRequest, InitResponse, Message, MessageId, getInitId, mkInitResponse, msgToString)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

data Node = Node
    { nodeId :: Text
    }

type ReqHandler req resp = req -> resp

spawnNode :: (FromJSON req, ToJSON resp) => ReqHandler req resp -> IO Node
spawnNode hlr = do
    logN "## Starting node"
    initReq <- awaitInitMessage

    logN $ "## Init msg: " <> show initReq
    let msgIdSeed = 1

    getNextId <- mkIdGen msgIdSeed

    respondInitOk (mkInitResponse initReq msgIdSeed)

    let node = Node (getInitId initReq)

    spawnMainLoop hlr -- TODO: async process, add Async to node, add node kill
    pure node

mkIdGen :: Integer -> IO (IO MessageId)
mkIdGen seed = pure (pure 123) -- TODO

awaitInitMessage :: IO (Message InitRequest)
awaitInitMessage = do
    m <- receiveMessage
    logN $ "## Raw Init: " <> show m

    case eitherDecode m of
        Left _err -> error "Parse err" -- <> LBS.pack  err
        Right m' -> do
            pure m'

respondInitOk :: Message InitResponse -> IO ()
respondInitOk m = do
    logN $ "## Repl Init: " <> msgToString m
    sendMessage (encode m)
    logN $ "## Repl Init done"

-- receiveMessage :: IO String
receiveMessage :: IO C8.ByteString
receiveMessage = fromString <$> getLine

sendMessage :: C8.ByteString -> IO ()
sendMessage msg = do
    let msg' = C8.unpack msg
    putStrLn msg' >> hFlush stdout

spawnMainLoop ::
    forall req resp. (FromJSON req, ToJSON resp) => ReqHandler req resp -> IO Void
spawnMainLoop hlr =
    -- TODO: each message handled async?
    forever $ do
        rawRequest <- receiveMessage

        logNB $ "## Raw request: " <> rawRequest

        case eitherDecode' rawRequest :: Either String req of
            Left e -> error e
            Right r -> do
                let resp = encode $ hlr r
                logNB $ "## Sending: " <> resp
                sendMessage resp

logN :: String -> IO ()
logN msg = hPutStrLn stderr msg >> hFlush stderr

logNB :: C8.ByteString -> IO ()
logNB = logN . C8.unpack
