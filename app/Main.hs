{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM_, forever)
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (fromMaybe)
import Network.Socket
import Network.Socket.ByteString
import System.IO (BufferMode (..), hSetBuffering, stdout)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    -- You can use print statements as follows for debugging, they'll be visible when running tests.
    BC.putStrLn "Logs from your program will appear here"

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
    let contentLines = BC.strip <$> BC.lines body
    BC.putStrLn ""
    forM_ contentLines BC.putStrLn

    let response = case BC.words <$> contentLines of
            ("GET" : "/" : _) : _ -> do
                mkResponse Status200 [] ""
            ("GET" : path : _) : _ ->
                if "/echo/" `BC.isPrefixOf` path
                    then echoResponse (BC.drop 6 path)
                    else mkResponse Status404 [] ""
            _ ->
                mkResponse Status404 [] ""

    sendAll clientSocket response

data StatusCode
    = Status200
    | Status404

toByteString :: StatusCode -> BC.ByteString
toByteString Status200 = "200 OK"
toByteString Status404 = "404 Not Found"

mkResponse :: StatusCode -> [BC.ByteString] -> BC.ByteString -> BC.ByteString
mkResponse status headers body =
    "HTTP/1.1 " <> toByteString status <> "\r\n" <> BC.intercalate "\r\n" headers <> "\r\n\r\n" <> body

echoResponse :: BC.ByteString -> BC.ByteString
echoResponse msg =
    mkResponse
        Status200
        [ "Content-Type: text/plain"
        , "Content-Length: " <> (BC.pack . show . BC.length) msg
        ]
        msg
