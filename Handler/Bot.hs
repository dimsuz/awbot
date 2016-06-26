{-# LANGUAGE DeriveGeneric #-}
module Handler.Bot where

import Import hiding (Update)
import Network.HTTP.Simple
import Data.Aeson as Json
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString.Lazy as B

data Message = Message {
    message_id :: Int
 -- , from
    , text :: Maybe Text
  } deriving (Generic, Show)

data Update = Update {
    update_id :: Int
  , message :: Maybe Message
  , edited_message :: Maybe Message
  } deriving (Generic, Show)

instance FromJSON Update
instance FromJSON Message

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
