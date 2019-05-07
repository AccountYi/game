module Adapter.Mysql.Auth where
  
import ClassyPrelude
import qualified Domain.Auth as D
import Text.StringRandom 
import Data.Pool
import Database.MySQL.Base
import qualified System.IO.Streams as Streams 
import System.IO.Unsafe (unsafePerformIO)
import qualified Adapter.Redis.Auth as R
import Control.Monad.Except

type MS m = (MonadIO m, MonadThrow m)

type SessionId = Text
type LoginError = Text

pool :: Pool MySQLConn
pool = unsafePerformIO $ createPool (connect $ defaultConnectInfo {ciUser = "root", ciPassword = "1", ciDatabase = "codegame"}) 
                      close 2 50000 10


withConn ::  MS m => (MySQLConn -> IO a) -> m a
withConn action = do
  liftIO $ withResource pool $ \conn -> action conn


-- 用户登录
login :: MS m =>  Text -> Text -> m (Either LoginError SessionId)
login email passw =  runExceptT $ do
  result <- withConn $ \conn -> query conn qry [One (MySQLText email),One (MySQLText passw)]
  res <- liftIO .Streams.toList $ snd result
  case res of
    [] ->  throwError "用户不存在"
    [_] -> lift $ R.newSession email
  where
    qry = "select * from user where user_name = ? and user_pawd = ?"


findEmailFromUserId :: MS m => D.Auth -> m (Maybe (D.UserId))
findEmailFromUserId auth =  do

  
  result <- withConn $ \conn -> query conn qry [Many [MySQLText "3", MySQLText "wangwu"]]
  --result <- withConn $ \conn -> query_ conn qry
  res <- liftIO .Streams.toList $ snd result
  traceM(show(res))
  return $ case fst result of
    ([_]) -> Just ("1")
    _ -> Nothing
  where
    qry = "select* from user where user_id = ? and user_name = ?"
