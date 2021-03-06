module Adapter.HTTP.Main where


import ClassyPrelude

import qualified Adapter.Mysql.Auth as M
import Network.Wai
import Network.Wai.Handler.Warp
import System.IO.Unsafe (unsafePerformIO)
import qualified Adapter.HTTP.API.Auth as Api
import Network.HTTP.Types (status200, unauthorized401, status404)



main ::Int -> IO ()
main port = do
  run port app

   
app :: Application
app req respond = respond $ 
    case pathInfo req of
      ["loginUser"] -> 
        unsafePerformIO $ Api.loginUser req 
      ["play"] -> 
            -- unsafePerformIO 函数是取出IO中的 Response
            unsafePerformIO $ Api.testParam req   
      ["init"] -> 
        unsafePerformIO $ Api.initCode req 
      ["static", subDir, fileName] -> 
            serveStatic subDir fileName  
      [] -> 
        resFile "text/html" "static/index.html"  
      _ -> res404      


resFile :: ByteString -> FilePath -> Response
resFile contentType filename = responseFile status200 [("Content-Type", contentType)] filename Nothing    

serveStatic :: Text -> Text -> Response
serveStatic subDir fName = 
  case sub of
    "js" -> serve "text/javascript"
    "css" -> serve "text/css"
    "images" -> serve "image/png"
    _ -> res404
  where serve mimeType = resFile mimeType $ concat ["static/", sub, "/", unpack fName]
        sub = unpack subDir

res404 :: Response
res404 = responseLBS status404 [] $ fromString "Not Found"                