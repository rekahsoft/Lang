-- (C) Copyright Collin Doering 2011
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- File: EchoServer.hs
-- Author: Collin J. Doering <rekahsoft@gmail.com>
-- Date: Nov  3, 2011

import Network
import System.IO
import Control.Concurrent

handleRequest :: Handle -> HostName -> PortNumber -> IO ()
handleRequest handle host port = do
  putStrLn $ "Recieved connection from " ++ host ++ " on port " ++ show port
  response <- hGetLine handle
  putStrLn $ "Recieved data \"" ++ response ++ "\" from client; echoing.."
  hPutStrLn handle response
  hClose handle

listenRec :: Socket -> IO ()
listenRec s = do
  (handle, host, port) <- accept s
  forkIO $ handleRequest handle host port
  listenRec s

main :: IO ()
main = withSocketsDo $ do
  socket <- listenOn (PortNumber 3556)
  putStrLn "EchoServer started on port 3556"
  listenRec socket

-- main = withSocketsDo $ do
--   sock <- listenOn (PortNumber 3556)
--   (h,host,port) <- accept sock
--   putStrLn ("Recieved connection from " ++ host ++ " on port " ++ show(port))
--   res <- hGetLine h
--   putStrLn ("Recieved data " ++ res ++ " from client; echoing..")
--   hPutStrLn h res
--   hClose h
--   sClose sock
