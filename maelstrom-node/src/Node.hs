module Node (
  spawnNode,
) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as C8
import Data.Text (Text)
import Data.Void (Void)
import GHC.Exts (fromString)
import Message (InitRequest, InitResponse, Message, MessageId, getInitId, mkInitResponse, msgToString)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

data Node = Node
  { nodeId :: Text
  }

spawnNode :: IO Node
spawnNode = do
  logN "## Starting node"
  initReq <- awaitInitMessage

  logN $ "## Init msg: " <> show initReq
  let msgIdSeed = 1

  getNextId <- mkIdGen msgIdSeed

  respondInitOk (mkInitResponse initReq msgIdSeed)

  let node = Node (getInitId initReq)

  spawnMainLoop

  pure node

mkIdGen :: Integer -> IO (IO MessageId)
mkIdGen seed = pure (pure 123) -- TODO

awaitInitMessage :: IO (Message InitRequest)
awaitInitMessage = do
  m <- fromString <$> getLine
  logN $ "## Raw Init: " <> show m

  case eitherDecode m of
    Left _err -> error "Parse err" -- <> LBS.pack  err
    Right m' -> do
      pure m'

respondInitOk :: Message InitResponse -> IO ()
respondInitOk m = do
  logN $ "## Repl Init: " <> msgToString m
  sendMessage (msgToString m)
  logN $ "## Repl Init done"

sendMessage :: String -> IO ()
sendMessage msg = do
  putStrLn msg >> hFlush stdout

-- let reader = getLine
--     writer msg = hPutStrLn stderr msg >> hFlush stderr
-- startNode reader writer

spawnMainLoop :: IO Void
spawnMainLoop =
  forever $ threadDelay 1_000_000

logN :: String -> IO ()
logN msg = hPutStrLn stderr msg >> hFlush stderr