-- By: uHOOCCOOHu @github

{-# LANGUAGE CPP        #-}
{-# LANGUAGE DataKinds  #-}
import System.Exit (die)
import System.IO
import System.IO.Error
import System.Environment (getEnv)
import Control.Exception (throw, catch, finally)
import Paths_Haskell_Console (version)
import Data.Version (showVersion)
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as TLS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL (toStrict)

strBanner =
  unlines [ "HostsTool-Console " ++ showVersion version ++ " for racaljk/hosts"
          , "  Powered By: uHOOCCOOHu"
          , "(https://github.com/HostsTools/cross-platform-console)"
          ]
strUpdating =
  unlines [ "Updating hosts..."
          , "正在更新Hosts..."
          ]
strUpdated =
  unlines [ "Hosts updated successfully."
          , "Hosts更新成功。"
          ]
strAlreadyLatest =
  unlines [ "Nothing was done. Hosts are already the latest version."
          , "Hosts已是最新版本，无需更新。"
          ]
strNoPermission =
  unlines [ "Permission denied. "
          , "Please run with sudo(linux) or administrator privileges(windows)."
          , "没有权限。"
          , "请使用sudu(linux)或管理员权限（windows）运行本程序。"
          ]
strNetError =
  unlines [ "Network error. Please check network setting and retry."
          , "网络错误。请检查网络配置并重试。"
          ]
strFileInUse =
  unlines [ "Hosts file is already in use. Please check your applications."
          , "Hosts文件正在被使用，请检查是否有其他软件正在编辑它。"
          ]
strPause =
  unlines [ "Press enter to exit..."
          , "按回车键退出..."
          ]

strTargetUrl = "https://raw.githubusercontent.com/racaljk/hosts/master/hosts"

strHostsBeginMark = "# Copyright (c) 2014-2016, racaljk."

getHostsPath :: IO String
#if defined(cygwin32_HOST_OS) || defined (mingw32_HOST_OS)
getHostsPath = do
  syspath <- getEnv "SystemRoot"
  return $ syspath ++ "/system32/drivers/etc/hosts"
#else
getHostsPath = return "/etc/hosts"
#endif

main :: IO ()
main = finally main' (putStrLn strPause >> getChar >> return ())

main' :: IO ()
main' = do
  hSetEncoding stdout localeEncoding
  putStrLn strBanner
  putStrLn strUpdating
  updated <- updateHosts `catch` onIOError
                         `catch` onHttpError
  putStrLn $ if updated then strUpdated else strAlreadyLatest
  where onIOError e
          | isPermissionError e   = die strNoPermission
          | isAlreadyInUseError e = die strFileInUse
          | otherwise             = die $ show e
        onHttpError = die . (strNetError ++) . show :: H.HttpException -> IO a

updateHosts :: IO Bool
updateHosts = getHostsPath >>= updateHosts'
  where updateHosts' path = withFile path ReadWriteMode process
        process hfile = do
          size <- hFileSize hfile
          old <- B.hGet hfile (fromIntegral size)
          new <- fetchHosts
          case transformHosts old new of
            Nothing -> return False
            Just content -> do
              hSetFileSize hfile 0
              hSeek hfile AbsoluteSeek 0
              B.hPut hfile content
              return True

transformHosts :: B.ByteString -> B.ByteString -> Maybe B.ByteString
transformHosts oldcontent newcontent
  | r == newcontent = Nothing
  | otherwise       = Just $ B.concat
    [ l
    , supplyLn l 3
    , newcontent
    ]
  where (l, r) = B.breakSubstring (B.pack strHostsBeginMark) oldcontent

supplyLn :: B.ByteString -> Int -> B.ByteString
supplyLn s n = repn $ (n-) $ B.length $ snd $ B.spanEnd (== '\n') s
  where repn n = B.replicate (max 0 n) '\n'

fetchHosts :: IO B.ByteString
fetchHosts = do
  request <- H.parseUrl strTargetUrl
  manager <- H.newManager TLS.tlsManagerSettings
  response <- H.httpLbs request manager
  return $ BL.toStrict $ H.responseBody response
