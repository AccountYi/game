module Adapter.HTTP.Common where

import ClassyPrelude
import Web.Scotty.Trans
import Blaze.ByteString.Builder (toLazyByteString)
import Web.Cookie
import Domain.Auth
import Data.Aeson hiding (json)
import Network.HTTP.Types.Status
import Data.Time.Lens
import Control.Exception.Lifted
import qualified Data.Map.Lazy as MAP


{- -- 解析cookie
getCookie ::  String -> IO (Maybe Text)
getCookie req = do
  let paramsMap = mapFromList $ requestHeaders req MAP.! "Cookie"
  return $ do
    cookie <- parseCookies . encodeUtf8 . toStrict <$> mCookieStr
    let bsKey = encodeUtf8 key
    val <- lookup bsKey cookie
    return $ decodeUtf8 val

 -}
-- 设置cookie
setSessionIdInCookie :: (MonadIO m,MonadBase IO m ) => SessionId -> m ByteString
setSessionIdInCookie sId = do
  curTime <- liftIO getCurrentTime
  evaluate $ toStrict . toLazyByteString . renderSetCookie $ def{ setCookieName = "sessionId",
                                                                  setCookieMaxAge = Just (86400000000000000*30),
                                                                  setCookieValue = encodeUtf8 sId , 
                                                                  setCookieExpires = Just $ modL month (+1) curTime}

{- getCurrentUserId :: (SessionRepo m, ScottyError e) => ActionT e m (Maybe UserId)
getCurrentUserId = do
  maySessionId <- getCookie "sId"
  case maySessionId of
    Nothing -> return Nothing
    Just sId -> lift $ resolveSessionId sId -}

{- reqCurrentUserId :: (SessionRepo m, ScottyError e) => ActionT e m UserId
reqCurrentUserId = do
  mayUserId <- getCurrentUserId
  case mayUserId of
    Nothing -> do
      status status401
      json ("AuthRequired" :: Text)
      finish
    Just userId ->
      return userId -}