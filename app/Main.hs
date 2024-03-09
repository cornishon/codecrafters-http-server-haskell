{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Char (toLower)
import Data.Map qualified as M
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO (BufferMode (..), hSetBuffering, stdout)

main :: IO ()
main = do
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
        bracket
            (accept serverSocket)
            (\(clientSocket, _) -> close clientSocket)
            handleConnection

handleConnection :: (Socket, SockAddr) -> IO ()
handleConnection (clientSocket, clientAddr) = do
    BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."

    body <- recv clientSocket 4096

    let request = parseRequest body
    BC.putStrLn $ BC.pack $ show request

    let response = case request of
            Just (Request "GET" path headers)
                | path == "/" ->
                    fromStatus Status200
                | "/echo" `BC.isPrefixOf` path ->
                    plainText (BC.drop 6 path)
                | path == "/user-agent"
                , Just userAgent <- M.lookup "user-agent" headers ->
                    plainText userAgent
                | otherwise ->
                    fromStatus Status404
            _ ->
                fromStatus Status400

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
