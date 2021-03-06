{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Bot where

import Import hiding (Update, User)
import Network.HTTP.Simple
import Data.Aeson as Json
import Data.Aeson.Types (Parser, parseEither, Options(..), camelTo2)
import qualified Data.ByteString.Lazy as B

data User = User {
    userId :: Int
  , firstName :: Text
  , lastName :: Maybe Text
  , username :: Maybe Text
  } deriving (Generic, Show)

data Chat = Chat {
  chatId :: Int64
  } deriving (Generic, Show)

data Message = Message {
    message_id :: Int
    , from :: User
    , chat :: Chat
    , text :: Maybe Text
    , entities :: Maybe [MessageEntity]
  } deriving (Generic, Show)

data EntityType = BotCommand | Mention | Unsupported
                  deriving Show

data MessageEntity = MessageEntity {
  entityType :: EntityType
  , offset :: Int
  , length :: Int
  } deriving (Generic, Show)

data Update = Update {
    update_id :: Int
  , message :: Maybe Message
  , edited_message :: Maybe Message
  } deriving (Generic, Show)

data SendMessageParams = SendMessageParams {
    sendMessageChatId :: Int64
  , sendMessageText :: Text
  } deriving (Generic, Show)

data GetUpdatesParams = GetUpdatesParams {
    getUpdatesOffset :: Int
  , getUpdatesTimeout :: Int
  } deriving (Generic, Show)

instance FromJSON Update
instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = \k ->
        if k == "userId" then (disambiguateIdModifier "userId" k) else (camelTo2 '_' k) }

instance FromJSON Chat where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = disambiguateIdModifier "chatId" }

instance FromJSON Message
instance FromJSON MessageEntity where
  parseJSON = withObject "message entity" $ \o -> do
    t <- parseEntityType =<< (o .: "type")
    offset <- o .: "offset"
    len <- o .: "length"
    return $ MessageEntity t offset len

instance ToJSON SendMessageParams where
  -- again, playing games with prefixes due to duplicate record fields errors
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = stripPrefixModifier "sendMessage"
    }

instance ToJSON GetUpdatesParams where
  -- again, playing games with prefixes due to duplicate record fields errors
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = stripPrefixModifier "getUpdates"
    }

-- maps a custom id key name to an "id" key. Needed because multiple
-- models use "id" in json and this plays bad with absence of DuplicateRecordFields extension
-- in current version of GHC...
disambiguateIdModifier :: String -> String -> String
disambiguateIdModifier idKey = \k -> if (k == idKey) then "id" else k

stripPrefixModifier :: String -> String -> String
stripPrefixModifier prefix key = camelTo2 '_' (maybe key id (stripPrefix prefix key))

parseEntityType :: Value -> Parser EntityType
parseEntityType = withText "entity type" $ \t -> do
  return $ case t of
    "bot_command" -> BotCommand
    "mention" -> Mention
    _ -> Unsupported


unwrap :: FromJSON a => String -> Value -> Parser [a]
unwrap name = withObject name $ \o -> o .: "result"

token :: Text
token = "200098948:AAE-bwgutgijahNPYpmQbKBARI7Rh-wFAbM"

addApiRequestParams :: ToJSON a => Request -> a -> Request
addApiRequestParams request p = setRequestBodyJSON p $ request { method = "POST" }

apiCall :: (MonadThrow m, MonadIO m, ToJSON a) => Text -> Maybe a -> m (Response B.ByteString)
apiCall methodName params = do
  let url = "https://api.telegram.org/bot" <> token <> "/" <> methodName
  liftIO $ putStrLn ("Sending request: " <> url)
  request <- parseRequest (unpack url)
  httpLBS $ maybe request (addApiRequestParams request) params

decodeUpdates :: B.ByteString -> Either String [Update]
decodeUpdates s = do
  decoded <- Json.eitherDecode s
  parseEither (unwrap "updates") decoded

sendMessage :: (MonadThrow m, MonadIO m) => Int64 -> Text -> m (Response B.ByteString)
sendMessage chatId text = apiCall "sendMessage" (Just (SendMessageParams chatId text))

getChatId :: Update -> Maybe Int64
getChatId u = do
  msg <- message u
  return $ (chatId . chat) msg

getUserFirstName :: Update -> Maybe Text
getUserFirstName u = do
  msg <- message u
  return $ (firstName . from) msg

getUserMessage :: Update -> Text
getUserMessage u = maybe "<nothing>" id $ do
  msg <- message u
  t <- text msg
  return t

isStartCommand :: Update -> Bool
isStartCommand u = True

handleUpdate :: Update -> Handler ()
handleUpdate u = do
  -- TODO if (message) then
    chatId <- maybe (fail "failed to extract chatId") return $ getChatId u
    userName <- maybe (fail "failed to extract userFirstName") return $ getUserFirstName u
    let message = getUserMessage u
    sendMessage chatId ("Dear " ++ userName ++ ", you said: " ++ message) >> putStrLn "message sent"
    return ()

markUpdateHandled :: Update -> Handler ()
markUpdateHandled u = do
  liftIO $ putStrLn $ "Marking update with id=" <> (pack . show . update_id) u <> " as handled"
  currentTime <- liftIO getCurrentTime
  runDB $ insert_ $ UpdateLog (update_id u) currentTime
  return ()

handleUpdates :: [Update] -> Handler ()
handleUpdates updates = mapM_ handleUpdateAndSave updates
  where handleUpdateAndSave u = do
          -- try to handle update and catch possible status errors.
          -- even if error happened, need to mark it as handled, otherwise
          -- it will always try to get the same update over and over again...
          catch (handleUpdate u) $ \(e :: HttpException) -> do
            putStrLn "error! caught an exception while handling update, skipping it"
            putStrLn $ "exception details: \n" <> (pack $ show e)
          markUpdateHandled u


buildGetUpdatesParams :: Maybe (Entity UpdateLog) -> Maybe GetUpdatesParams
buildGetUpdatesParams updateEntity = do
  entity <- updateEntity
  let lastUpdate = entityVal entity
      nextId = (updateLogUpdateId lastUpdate) + 1
  return $ GetUpdatesParams nextId 0


getBotRefreshR :: Handler ()
getBotRefreshR = do
  lastUpdate <- runDB $ selectFirst [] [Desc UpdateLogUpdateId]
  let updateParams = buildGetUpdatesParams lastUpdate
  response <- apiCall "getUpdates" updateParams
  putStrLn $ "The status code was: " ++
               (pack . show) (getResponseStatusCode response)
  either fail handleUpdates (decodeUpdates (getResponseBody response))
  return ()
