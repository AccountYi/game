module Adapter.HTTP.API.Auth where

import ClassyPrelude
import Adapter.HTTP.Common
import Network.Wai
import Network.Wai.Handler.Warp
import Data.ByteString.Builder (lazyByteString)
import Network.Wai.Parse (parseRequestBody,lbsBackEnd)
import Network.HTTP.Types (status200, unauthorized401, status404)
import qualified Adapter.Mysql.Auth as M
import qualified Adapter.Redis.Auth as R
import qualified Data.Map.Lazy as MAP
import Domain.Auth
import Domain.Model
import qualified Adapter.HTTP.API.Tool as Tool
import Data.Aeson (encode,decode)
import System.Process
import qualified System.IO.Strict as IS (hGetContents)
import qualified Data.List as LIST
import System.IO as IO
import System.Timeout
import qualified Adapter.HTTP.Common as C
-- 获取初始化代码
initCode ::Request ->IO Response
initCode req = do
    (params, _) <- parseRequestBody lbsBackEnd req
    let paramsMap = mapFromList params :: Map ByteString ByteString
    let language =(paramsMap MAP.! "language")
    -- FIXME 文件名
    let pathName = "./static/init/"++ case language of
                                          -- "python"->"python.py" 
                                          "java"->"Solution.java"
                                          "haskell"->"haskell.hs"
                                          _->"python.py"
    inpStr <- IO.readFile pathName
    -- ,language=if language == "" then "python" else language
    let codeList= encode (CodeList {codeList = inpStr})
    return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ codeList

--用户登录
loginUser :: Request ->IO Response
loginUser req = do
    (params, _) <- parseRequestBody lbsBackEnd req
    let abc = requestHeaders req
    traceM(show(abc))
    let paramsMap = mapFromList params :: Map ByteString ByteString
    result <-M.login (decodeUtf8 $ paramsMap MAP.! "email") $ decodeUtf8 $ paramsMap MAP.! "passw"    
    case result of
        Left mesg -> return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ encode (CodeOutput {output= mesg, message="", found="", expected="", errMessage=""})
        Right sessionId ->  do
            cookies <- C.setSessionIdInCookie sessionId
            traceM(show(cookies))
            return $ responseBuilder status200 [("Content-Type","application/json"),("Set-Cookies",cookies)] $ lazyByteString $ encode (CodeOutput {output= "欢迎登录", message="", found="", expected="", errMessage=""})


-- 提交代码验证是否正确
testParam ::Request ->IO Response
testParam req = do
    (params, _) <- parseRequestBody lbsBackEnd req
    --返回代码写入文件的路径和shell脚本在哪个路径下运行的命令
    let email = either undefined id $ mkEmail "randEmail@qq.com"
        passw = either undefined id $ mkPassword "1234ABCDefgh"
        auth = Auth email passw
    M.findEmailFromUserId auth
    let paramsMap = mapFromList params :: Map ByteString ByteString
        language = (unpack . decodeUtf8) (paramsMap MAP.! "language")
        code = (unpack . decodeUtf8) (paramsMap MAP.! "code")
        testIndex = (unpack . decodeUtf8) (paramsMap MAP.! "testIndex")
        languageSetting = Tool.getLanguageSetting  language code
    -- 写入文件文件名不存在的时候会新建，每次都会重新写入
    outh <- IO.openFile (LIST.head languageSetting) WriteMode
    hPutStrLn outh (languageSetting LIST.!! 2)
    IO.hClose outh
    -- 用shell命令去给定位置找到文件运行脚本。得到输出的句柄。（输入句柄，输出句柄，错误句柄，不详）
    --获取输入参数的文件路径
    let factorPath = LIST.head $ Tool.getPath testIndex
    inh <- openFile factorPath ReadMode
    (_,Just hout,Just err,_) <- createProcess (shell (LIST.last languageSetting)){cwd=Just(languageSetting LIST.!! 1),std_in = UseHandle inh,std_out=CreatePipe,std_err=CreatePipe}
    hClose inh
    -- 获取文件运行的结果
    content <- timeout 2000000 (IS.hGetContents hout)
    errMessage <- IS.hGetContents err
    case content of
      Nothing     -> 
        return $ responseBuilder status200 [("Content-Type","application/json")] $ lazyByteString $ encode (CodeOutput {output= fromString "Timeout: your program did not provide an input in due time.", message="Failure", found="", expected="", errMessage= fromString errMessage})
      Just value  -> do 
        let contents = LIST.lines value
        -- 读取文件中保存的正确答案
        inpStr <- IO.readFile (LIST.last $ Tool.getPath ((unpack . decodeUtf8) $ paramsMap MAP.! "testIndex"))
        let inpStrs = LIST.lines inpStr
        let codeOutput =  if contents == inpStrs
                          then encode (CodeOutput {output=fromString value, message="Success", found="", expected="", errMessage= fromString errMessage})
                          else encode (CodeOutput {output=fromString value, message="Failure", found=LIST.head contents, expected=LIST.head inpStrs, errMessage= fromString errMessage})
                  -- 打印数据的方法 traceM(show(content))
        return $ responseBuilder status200 [("Content-Type","application/json"),("Set-Cookie","123")] $ lazyByteString $ codeOutput  


 
