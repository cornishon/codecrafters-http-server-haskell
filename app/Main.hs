{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM_, forever)
import Data.ByteString.Char8 qualified as BC
import Network.Socket
import Network.Socket.ByteString
import System.IO (BufferMode (..), hSetBuffering, stdout)

status200 :: BC.ByteString
status200 = "HTTP/1.1 200 OK\r\n\r\n"

status404 :: BC.ByteString
status404 = "HTTP/1.1 404 Not Found\r\n\r\n"

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
    forever
        $ bracket (accept serverSocket) (\(clientSocket, _) -> close clientSocket)
        $ \(clientSocket, clientAddr) -> do
            BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."

            body <- recv clientSocket 4096
            let contentLines = BC.strip <$> BC.lines body
            BC.putStrLn ""
            forM_ contentLines BC.putStrLn

            _ <- case BC.words <$> contentLines of
                ("GET" : "/" : _) : _ -> do
                    BC.putStrLn "path: /"
                    sendAll clientSocket status200
                ("GET" : path : _) : _ -> do
                    BC.putStrLn $ "path: " <> path
                    sendAll clientSocket status404
                _ ->
                    sendAll clientSocket status404

            close clientSocket
