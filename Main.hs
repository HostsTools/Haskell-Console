-- By: uHOOCCOOHu @github

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}
import System.IO
import System.Directory
import Control.Monad
import Paths_Haskell_Console (version)
import Data.Version (showVersion)
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as TLS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL (toStrict)
#if defined(cygwin32_HOST_OS) || defined (mingw32_HOST_OS)
import System.Environment (getEnv)
#endif

strBanner :: String
strUpdated :: String
strNoUpdate :: String
strBanner = unlines
            [ "HostsTool-Console " ++ showVersion version ++ " for racaljk/hosts"
            , "  Powered By: uHOOCCOOHu"
            , "(https://github.com/HostsTools/cross-platform-console)"
            ]
strUpdated  = "Done."
strNoUpdate = "Nothing was done. Hosts are already the latest version."

strHostsUrl :: String
strHostsBeginMark :: B.ByteString
strHostsUrl = "https://raw.githubusercontent.com/racaljk/hosts/master/hosts"
strHostsBeginMark = B.pack "# Copyright (c) 2014-2016, racaljk."

getHostsPath :: IO FilePath
#if defined(cygwin32_HOST_OS) || defined (mingw32_HOST_OS)
getHostsPath = (++ "/system32/drivers/etc/hosts") <$> getEnv "SystemRoot"
#else
getHostsPath = return "/etc/hosts"
#endif

main :: IO ()
main = do
  putStrLn strBanner
  hostspath <- getHostsPath
  ensureRWPermission hostspath
  newhosts <- fetchURL strHostsUrl
  updated <- updateHosts hostspath newhosts
  putStrLn $ if updated then strUpdated else strNoUpdate

ensureRWPermission :: FilePath -> IO ()
ensureRWPermission path = do
  per <- getPermissions path
  unless (readable per && writable per) $
    setPermissions path per{readable = True, writable = True}

updateHosts :: FilePath -> B.ByteString -> IO Bool
updateHosts path new = withFile path ReadWriteMode process
  where process hfile = do
          size <- hFileSize hfile
          old <- B.hGet hfile (fromIntegral size)
          let (l, r) = B.breakSubstring strHostsBeginMark old
          if translateINL r == new then
            return False
          else do
            let llen = fromIntegral $ B.length l
            hSetFileSize hfile llen
            hSeek hfile AbsoluteSeek llen
            B.hPut hfile $ translateONL $
              B.append (supplyLn (translateINL l) 5) new
            return True

supplyLn :: B.ByteString -> Int -> B.ByteString
supplyLn s n = repn $ (n-) $ B.length $ snd $ B.spanEnd (== '\n') s
  where repn x = B.replicate (max 0 x) '\n'

fetchURL :: String -> IO B.ByteString
fetchURL url = do
  request <- H.parseUrl url
  manager <- H.newManager TLS.tlsManagerSettings
  response <- H.httpLbs request manager
  return $ translateINL $ BL.toStrict $ H.responseBody response

translateINL :: B.ByteString -> B.ByteString
translateINL = B.concat . B.split '\r'

translateONL :: B.ByteString -> B.ByteString
translateONL = case nativeNewline of
  LF    -> id
  CRLF  -> B.intercalate "\r\n" . B.split '\n'
