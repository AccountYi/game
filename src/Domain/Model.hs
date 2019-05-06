module Domain.Model where
import Data.Aeson
import ClassyPrelude
import Data.Text    
import GHC.Generics
data CodeOutput = CodeOutput {
      output  :: Text,
      message  :: String,
      found  :: String,
      expected  :: String,
      errMessage  :: Text
    } deriving (Generic,Show)
instance ToJSON CodeOutput 
instance FromJSON CodeOutput     
-- 保存代码的类型
data CodeList = CodeList {
      codeList :: String
    } deriving (Generic,Show)
instance ToJSON CodeList 
instance FromJSON CodeList

