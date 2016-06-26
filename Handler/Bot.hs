{-# LANGUAGE DeriveGeneric #-}
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

instance FromJSON Update
instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = \k ->
        if k == "userId" then (disambiguateId "userId" k) else (camelTo2 '_' k) }

instance FromJSON Chat where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = disambiguateId "chatId" }

instance FromJSON Message
instance FromJSON MessageEntity where
  parseJSON = withObject "message entity" $ \o -> do
    t <- parseEntityType =<< (o .: "type")
    offset <- o .: "offset"
    len <- o .: "length"
    return $ MessageEntity t offset len

-- maps a custom id key name to an "id" key. Needed because multiple
-- models use "id" in json and this plays bad with absence of DuplicateRecordFields extension
-- in current version of GHC...
disambiguateId :: String -> String -> String
disambiguateId idKey = \k -> if (k == idKey) then "id" else k

parseEntityType :: Value -> Parser EntityType
parseEntityType = withText "entity type" $ \t -> do
  return $ case t of
    "bot_command" -> BotCommand
    "mention" -> Mention
    _ -> Unsupported


unwrap :: FromJSON a => String -> Value -> Parser [a]
unwrap name = withObject name $ \o -> o .: "result"

token :: String
token = "200098948:AAE-bwgutgijahNPYpmQbKBARI7Rh-wFAbM"

apiCall :: (MonadThrow m, MonadIO m) => String -> m (Response B.ByteString)
apiCall methodName = do
  request <- parseUrl ("https://api.telegram.org/bot" `mappend` token `mappend` "/" `mappend` methodName)
  httpLBS request

decodeUpdates :: B.ByteString -> Either String [Update]
decodeUpdates s = do
  decoded <- Json.eitherDecode s
  parseEither (unwrap "updates") decoded

getBotRefreshR :: Handler ()
getBotRefreshR = do
  response <- apiCall "getUpdates"
  putStrLn $ "The status code was: " ++
               (pack . show) (getResponseStatusCode response)
  print $ decodeUpdates (getResponseBody response)
