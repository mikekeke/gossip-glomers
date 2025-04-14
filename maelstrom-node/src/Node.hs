{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
module Node (
  spawnNode,
  ReqHandler,
  debug,
) where

import Control.Concurrent (newMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (forever, guard)
import Data.Aeson (FromJSON, ToJSON, Value (String), eitherDecode, eitherDecode', encode)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as AKM
import Data.ByteString.Lazy.Char8 qualified as C8
import Data.Foldable.WithIndex (FoldableWithIndex (ifoldr'))
import Data.Text (Text)
import Data.Void (Void)
import GHC.Exts (fromString)
import Message (InitRequest, InitResponse, Message (Message), MessageId, getInitId, mkInitResponse, msgToString, CommonMsg)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Debug.Trace (traceM)
import GHC.Generics (Generic)
import Data.Char (toLower)
import GHC.Unicode (isUpper)
import Data.Aeson.TH (Options(fieldLabelModifier))
import Data.Aeson.Types qualified as AT

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

  initMsgId <- getNextId
  guard (initMsgId == msgIdSeed)

  respondInitOk (mkInitResponse initReq msgIdSeed)

  let node = Node (getInitId initReq)

  spawnMainLoop hlr getNextId -- TODO: async process, add Async to node, add node kill
  pure node

mkIdGen :: Integer -> IO (IO MessageId)
mkIdGen seed = do
  -- i <- newMVar# seed
  mvi <- newMVar seed
  pure $ do
    ci <- takeMVar mvi
    putMVar mvi (succ ci)
    pure ci

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
  forall req resp.
  (FromJSON req, ToJSON resp) =>
  ReqHandler req resp ->
  IO MessageId ->
  IO Void
spawnMainLoop msgHandler getNextId =
  -- TODO: each message handled async?
  forever $ do
    rawRequest <- receiveMessage

    logNB $ "## Raw request: " <> rawRequest

    case eitherDecode' rawRequest :: Either String (Message Aeson.Object) of
      Left e -> error e
      Right (Message from to b) -> do
        msgId <- getNextId
        logN $ "Parsed req body: " <> show b
        let 
          (common, users) = splitBody b

          -- _ = AT.parseEither (a -> Parser b) a
        users' <- case Aeson.fromJSON @req (Aeson.toJSON users) of
            AT.Error s -> error $ "users re-jsoning failed: " <> s
            AT.Success a -> pure a

        let  resp = msgHandler users'


        error "ffff"
        -- let resp = msgHandler b

        -- sendMessage $ encode (Message to from resp)
 where

splitBody :: Aeson.Object -> (Aeson.Object, Aeson.Object)
splitBody = ifoldr' f (mempty, mempty) -- TODO: toJSON both values in pair?
 where
  -- :: Key -> Value -> (Object, Object) -> (Object, Object)
  f k v (common, user) = case k of
    "msg_id" -> (AKM.singleton k v <> common, user)
    _ -> (common, AKM.singleton k v <> user)





-- $(deriveJSON defaultOptions{fieldLabelModifier = snakeCase} ''CommonMsg)
-- $(deriveJSON defaultOptions ''CommonMsg)

logN :: String -> IO ()
logN msg = hPutStrLn stderr msg >> hFlush stderr

logNB :: C8.ByteString -> IO ()
logNB = logN . C8.unpack

-- >>> debug
-- "Success (CommonMsg {cmMsgId = 1})"
debug :: IO String
debug = do
  (Message _ _ body) <-
    Aeson.eitherDecodeFileStrict' @(Message Aeson.Object) "req.json"
      >>= either error pure
  let (common, users) = splitBody body
  pure $ show $ Aeson.fromJSON @CommonMsg (Aeson.toJSON common)


-- snakeCase :: String -> String
-- snakeCase = symbCase '_'

-- symbCase :: Char -> (String -> String)
-- symbCase sym =  u . applyFirst toLower
--   where u []                       = []
--         u (x:xs) | isUpper x = sym : toLower x : u xs
--                  | otherwise = x : u xs

-- applyFirst :: (Char -> Char) -> String -> String
-- applyFirst _ []     = []
-- applyFirst f [x]    = [f x]
-- applyFirst f (x:xs) = f x: xs
