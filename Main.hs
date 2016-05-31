-- By: uHOOCCOOHu @github

{-# LANGUAGE CPP        #-}
{-# LANGUAGE DataKinds  #-}
import System.Exit (die)
import System.IO
import System.IO.Error
import System.Environment (getEnv)
import Control.Exception (throw, catch)
import Text.Read (readMaybe)
import Data.String (fromString)
import Data.Time.Clock (UTCTime)
import qualified GitHub as G
import qualified Data.Vector as V (find)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base64 as Base64 (decode)

strBanner =
  unlines [ "HostsTool-Console v0.1 for racaljk/hosts"
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
  unlines [ "Nothing is done. Hosts are already the latest version."
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
  unlines [ "Hosts is already in use. Please check your applications."
          , "Hosts正在被使用，请检查是否有其他软件正在编辑它。"
          ]

strOwner = fromString "racaljk"
strRepo  = fromString "hosts"
strFile  = fromString "hosts"

strHostsBeginMark  = fromString "# HOSTSTOOL: DO NOT EDIT BELOW!\n# "
strHostsBeginMark' = fromString "# Copyright (c) 2014-2016, racaljk.\n"

getHostsPath :: IO String
#if defined(cygwin32_HOST_OS) || defined (mingw32_HOST_OS)
getHostsPath = do
  syspath <- getEnv "SystemRoot"
  return $ syspath ++ "/system32/drivers/etc/hosts"
#else
getHostsPath = return "/etc/hosts"
#endif

main :: IO ()
main = do
  putStrLn strBanner
  putStrLn strUpdating
  updated <- updateHosts `catch` onIOError
                         `catch` onNetError
  putStrLn $ if updated then strUpdated else strAlreadyLatest
  where onIOError e
          | isPermissionError e   = die strNoPermission
          | isAlreadyInUseError e = die strFileInUse
          | otherwise             = die $ show e
        onNetError = const $ die strNetError :: G.Error -> IO Bool

updateHosts :: IO Bool
updateHosts = getHostsPath >>= updateHosts'
  where updateHosts' path = withFile path ReadWriteMode process
        process hfile = do
          size <- hFileSize hfile
          old <- B.hGet hfile (fromIntegral size)
          new' <- transformHosts old
          case new' of
            Nothing -> return False
            Just new -> do
              hSetFileSize hfile 0
              hSeek hfile AbsoluteSeek 0
              B.hPut hfile new
              return True

transformHosts :: B.ByteString -> IO (Maybe B.ByteString)
transformHosts oldcontent = do
  let (l , r) = B.breakSubstring strHostsBeginMark  oldcontent
  let (l', _) = B.breakSubstring strHostsBeginMark' oldcontent
  let old = if B.null r then l' else l
  let updtime = readMaybe $ B.unpack $
                B.takeWhile (/= '\n') $
                B.drop (B.length strHostsBeginMark) r
  commit' <- fetchLastHostsCommitSince updtime
  case commit' of
    Nothing -> return Nothing
    Just commit -> do
      content <- fetchHostsFromCommit commit
      return $ Just $ B.concat
        [ old
        , reverseLn old 3
        , strHostsBeginMark, B.pack $ show $ timeOfCommit commit, B.pack "\n"
        , content
        ]
  where reverseLn s n = if l < n || B.drop (l - n) s /= ns then ns else B.empty
          where ns = B.replicate n '\n'
                l = B.length s

fetchHostsFromCommit :: G.Commit -> IO B.ByteString
fetchHostsFromCommit commit = do
  let treesha = G.treeSha $ G.gitCommitTree $ G.commitGitCommit commit
  tree <- execGRequest $ G.treeR strOwner strRepo treesha
  let Just blob = V.find ((== strFile) . G.gitTreePath) (G.treeGitTrees tree)
  let blobsha = fromString $ T.unpack $ G.untagName $ G.gitTreeSha blob
  cont <- execGRequest $ G.blobR strOwner strRepo blobsha
  let Right r = Base64.decode $ T.encodeUtf8 $ T.concat . T.lines $
                G.blobContent cont in
    return r

fetchLastHostsCommitSince :: Maybe UTCTime -> IO (Maybe G.Commit)
fetchLastHostsCommitSince time = do
  ret <- execGRequest $
    G.commitsWithOptionsForR strOwner strRepo (Just 1) (opts time)
  return $ V.find ((/= time) . Just . timeOfCommit) ret
  where opts Nothing = [G.CommitQueryPath strFile]
        opts (Just time) = G.CommitQuerySince time : opts Nothing

timeOfCommit :: G.Commit -> UTCTime
timeOfCommit = G.gitUserDate . G.gitCommitCommitter . G.commitGitCommit

execGRequest :: G.Request 'False a -> IO a
execGRequest req = either throw id <$> G.executeRequest' req
