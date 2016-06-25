module Handler.Bot where

import Import
import Network.HTTP.Simple
import Data.Aeson as Json

token :: String
token = "200098948:AAE-bwgutgijahNPYpmQbKBARI7Rh-wFAbM"

apiCall :: (MonadThrow m, MonadIO m, FromJSON a) => String -> m (Response a)
apiCall methodName = do
  request <- parseUrl ("https://api.telegram.org/bot" `mappend` token `mappend` "/" `mappend` methodName)
  httpJSON request

getBotRefreshR :: Handler ()
getBotRefreshR = do
  response <- apiCall "getUpdates" :: Handler (Response Value)
  putStrLn $ "The status code was: " ++
               (pack . show) (getResponseStatusCode response)
  print $ Json.encode (getResponseBody response)
