module Handler.Bot where

import Import

token :: Text
token = "200098948:AAE-bwgutgijahNPYpmQbKBARI7Rh-wFAbM"

getBotRefreshR :: Handler ()
getBotRefreshR = sendResponse ()
