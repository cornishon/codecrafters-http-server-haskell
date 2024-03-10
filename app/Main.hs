{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkFinally)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Char (toLower)
import Data.Map qualified as M
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (BufferMode (..), IOMode (ReadMode), hSetBuffering, openBinaryFile, stdout)

getDirectoryfromArgs :: IO (Maybe FilePath)
getDirectoryfromArgs = do
    args <- getArgs
    case args of
        ["--directory", dir] -> pure $ Just dir
        _ -> pure Nothing

main :: IO ()
main = do
    directory <- getDirectoryfromArgs
    BC.putStrLn $ "directory: " <> BC.pack (show directory)

    hSetBuffering stdout LineBuffering

    let host = "127.0.0.1"
        port = "4221"

    BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port

    -- Get address information for the given host and port
    addrInfo <- getAddrInfo Nothing (Just host) (Just port)

    serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol

    -- reuse local address to prevent "resource busy" errors
    setSocketOption serverSocket ReuseAddr 1
    withFdSocket serverSocket setCloseOnExecIfNeeded

    bind serverSocket $ addrAddress $ head addrInfo
    listen serverSocket 5

    -- Accept connections and handle them forever
    forever $ do
        (clientSocket, clientAddr) <- accept serverSocket
        handleConnection clientSocket clientAddr directory
            `forkFinally` \_ -> close clientSocket

handleConnection :: Socket -> SockAddr -> Maybe FilePath -> IO ()
handleConnection clientSocket clientAddr maybeDir = do
    BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."

    buffer <- recv clientSocket 4096

    let request = parseRequest buffer
    BC.putStrLn $ BC.pack $ show request

    response <- case request of
        Just (Request "GET" path headers)
            | "/" == path ->
                pure $ fromStatus Status200
            | "/echo/" `BC.isPrefixOf` path ->
                pure $ plainText (BC.drop 6 path)
            | "/files/" `BC.isPrefixOf` path
            , Just dir <- maybeDir -> do
                let filepath = dir <> "/" <> BC.unpack (BC.drop 7 path)
                exists <- doesFileExist filepath
                if exists
                    then do
                        h <- openBinaryFile filepath ReadMode
                        contents <- BC.hGetContents h
                        pure $ octetStream contents
                    else pure $ fromStatus Status404
            | "/user-agent" == path
            , Just userAgent <- M.lookup "user-agent" headers ->
                pure $ plainText userAgent
            | otherwise ->
                pure $ fromStatus Status404
        _ ->
            pure $ fromStatus Status400

    sendAll clientSocket (renderResponse response)

data StatusCode
    = Status200
    | Status400
    | Status404
    deriving (Show)

renderStatus :: StatusCode -> ByteString
renderStatus Status200 = "200 OK"
renderStatus Status400 = "400 Bad Request"
renderStatus Status404 = "404 Not Found"

type HeaderMap = M.Map ByteString ByteString

data Response = Response
    { resCode :: StatusCode
    , resHeaders :: HeaderMap
    , resBody :: ByteString
    }
    deriving (Show)

renderResponse :: Response -> ByteString
renderResponse (Response status headerMap body) =
    let headers = (\(k, v) -> k <> ":" <> v) <$> M.toList headerMap
     in "HTTP/1.1 " <> renderStatus status <> "\r\n" <> BC.intercalate "\r\n" headers <> "\r\n\r\n" <> body

fromStatus :: StatusCode -> Response
fromStatus code = Response code M.empty ""

plainText :: ByteString -> Response
plainText msg = Response Status200 headers msg
  where
    msgLength = (BC.pack . show . BC.length) msg
    headers = M.fromList [("Content-Type", "text/plain"), ("Content-Length", msgLength)]

octetStream :: ByteString -> Response
octetStream content = Response Status200 headers content
  where
    contentLen = (BC.pack . show . BC.length) content
    headers = M.fromList [("Content-Type", "application/octet-stream"), ("Content-Length", contentLen)]

data Request = Request
    { reqMethod :: ByteString
    , reqPath :: ByteString
    , reqHeaders :: HeaderMap
    }
    deriving (Show)

parseRequest :: ByteString -> Maybe Request
parseRequest content = do
    let (startLine, headerMap) = BC.breakSubstring "\r\n" content
    (method, path) <- case BC.words startLine of
        [m, p, _] -> Just (m, p)
        _ -> Nothing
    pure $ Request method path (parseHeaders headerMap)

parseHeaders :: ByteString -> HeaderMap
parseHeaders hs = go (breakLine hs) M.empty
  where
    breakLine = BC.breakSubstring "\r\n" . BC.strip
    go ("", _) acc = acc
    go (kv, rest) acc =
        let (k, v) = BC.break (== ':') kv
            k' = BC.map toLower (BC.strip k)
            v' = BC.strip (BC.drop 1 v)
         in go (breakLine rest) (M.insert k' v' acc)
