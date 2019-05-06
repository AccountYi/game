module Adapter.Redis.Auth where
  
import ClassyPrelude
import qualified Domain.Auth as D
import Text.StringRandom
import qualified Database.Redis as R

import System.IO.Unsafe (unsafePerformIO)
type State = R.Connection

type Redis  m = (MonadIO m, MonadThrow m)

    
redConn :: IO R.Connection
redConn =  case R.parseConnectInfo "redis://localhost:6379/0" of
  Left _ ->
    throwString $ "Invalid Redis conn URL:"
  Right connInfo -> do
    R.checkedConnect connInfo
withConn :: Redis  m => R.Redis a -> m a
withConn action = do
  liftIO $ R.runRedis (unsafePerformIO redConn) action

newSession :: Redis  m => D.UserId -> m D.SessionId
newSession userId = do
  sId <- liftIO $ stringRandomIO "[a-zA-Z0-9]{32}"
  result <- withConn $ R.set (encodeUtf8 sId) (fromString . show $ userId) 
  traceM(show(result))
  case result of
    Right R.Ok -> return sId
    err -> throwString $ "Unexpected redis error: " <> show err

findUserIdBySessionId :: Redis  m => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId sId = do
  result <- withConn $ R.get (encodeUtf8 sId)
  traceM(show(result))
  return $ case result of
    Right (Just uIdStr) -> readMay . unpack . decodeUtf8 $ uIdStr
    err -> throwString $ "Unexpected redis error: " <> show err