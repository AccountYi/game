
module Domain.Auth ( 
  -- * Types
  Auth(..),
  Email(rawEmail),
  mkEmail,
  Password(rawPassword),
  mkPassword,
  UserId,
  RegistrationSuccess,
  SessionId,
  RegistrationError(..),
  EmailVerificationError(..),
  LoginError(..),
  -- * Ports
  AuthRepo(..),
  SessionRepo(..),
  -- * Use cases
  login,
  resolveSessionId,
  getUser
) where

import ClassyPrelude
import Domain.Validation
import Text.Regex.PCRE.Heavy
import Control.Monad.Except

newtype Email = Email { rawEmail :: Text } deriving (Show, Eq, Ord)


mkEmail :: Text -> Either [Text] Email
mkEmail =
  validate Email
    [ regexMatches
        [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
        "Not a valid email"
    ]

newtype Password = Password { rawPassword :: Text } deriving (Show, Eq)


mkPassword :: Text -> Either [Text] Password
mkPassword =
  validate Password
    [ lengthBetween 5 50 "Should between 5 and 50"
    , regexMatches [re|\d|] "Should contain number"
    , regexMatches [re|[A-Z]|] "Should contain uppercase letter"
    , regexMatches [re|[a-z]|] "Should contain lowercase letter"
    ]
-- 用户的电子邮件 和 密码    
data Auth = Auth
  { authEmail :: Email
  , authPassword :: Password
  } deriving (Show, Eq)

type UserId = Int

type RegistrationSuccess = Text
type SessionId = Text

data RegistrationError
  = RegistrationErrorEmailTaken
  deriving (Show, Eq)

data EmailVerificationError
  = EmailVerificationErrorInvalidCode
  deriving (Show, Eq)

data LoginError
  = LoginErrorInvalidAuth
  | LoginErrorEmailNotVerified
  deriving (Show, Eq)


class (Monad m) => AuthRepo m where
  --addAuth函数是将Auth添加到存储库   注册错误         注册成功
  addAuth :: Auth -> m (Either RegistrationError RegistrationSuccess)

   -- Bool部分用于表示电子邮件是否已经过验证。我们需要它来满足我们要拒绝使用未经验证的电子邮件登录的要求。
  findUserByAuth :: Auth -> m (Maybe UserId)
   -- 通过UserId找到电子邮箱
  findEmailFromUserId :: UserId -> m (Maybe Email)

class (Monad m) => SessionRepo m where
  -- 除了身份验证存储库之外，login还需要与会话存储库进行交互。当用户登录时，我们需要为该用户创建一个新会话。这样做的功能是newSession。
  newSession :: UserId -> m SessionId
   -- 通过SessionId找到UserId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId) 


-- 登录功能  我们首先使用findUserByAuth函数从Auth中查找用户ID。结果是模式匹配;在Nothing的情况下，我们发出无效的身份验证错误信号。
-- 在Just _ 的情况下，我们使用newSession函数创建一个新会话。
login :: (AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing -> throwError LoginErrorInvalidAuth
    Just uId -> lift $ newSession uId

--该函数是把SessionId解析回UserId 该函数现在只是findUserBySessionId的同义词。这似乎是不必要的样板。但是，我们暂时保留它以保持一致性和可能的​​未来功能添加。
resolveSessionId :: (SessionRepo m) => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId

getUser :: (AuthRepo m) => UserId -> m (Maybe Email)
getUser = findEmailFromUserId
