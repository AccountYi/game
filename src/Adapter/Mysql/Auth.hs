module Adapter.Mysql.Auth where
  
import ClassyPrelude
import qualified Domain.Auth as D
import Text.StringRandom 
import Data.Pool
import Database.MySQL.Base
import qualified System.IO.Streams as Streams 
import System.IO.Unsafe (unsafePerformIO)



type MS m = (MonadIO m, MonadThrow m)

type SessionId = Text
type LoginError = Text

pool :: Pool MySQLConn
pool = unsafePerformIO $ createPool (connect $ defaultConnectInfo {ciUser = "root", ciPassword = "1", ciDatabase = "codegame"}) 
                      close 2 50000 10


withConn ::  MS m => (MySQLConn -> IO a) -> m a
withConn action = do
  liftIO $ withResource pool $ \conn -> action conn


{- -- 用户登录
login :: Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing -> throwError LoginErrorInvalidAuth
    Just uId -> lift $ newSession uId -}



findEmailFromUserId :: MS m => D.Auth -> m (Maybe (D.UserId))
findEmailFromUserId auth =  do
  result <- withConn $ \conn -> query_ conn qry
  res <- liftIO .Streams.toList $ snd result
  traceM(show(res))
  return $ case fst result of
    ([a]) -> Just (1)
    _ -> Nothing
  where
    qry = "select* from user"
