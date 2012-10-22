module Main where

import Data.List
import System.IO
import Network
import System.Environment (getArgs)

-- Thrift libraries
import Thrift
import Thrift.Transport.Handle
import Thrift.Protocol
import Thrift.Protocol.Binary
import Thrift.Server

-- Generated Thrift modules
import Test_Types

-- Constants

port :: PortNumber
port = 4390

testdata :: Work
testdata = Work {
  f_Work_num1 = Just 1,
  f_Work_num2 = Just 2,
  f_Work_op = Just ADD,
  f_Work_comment = Just "Foo!"
  }

testdata2 :: Work
testdata2 = Work {
  f_Work_num1 = Just 10,
  f_Work_num2 = Just 20,
  f_Work_op = Just SUBTRACT,
  f_Work_comment = Just "Bar!"
  }

-- Functions

serverFunc :: a -> (BinaryProtocol Handle, BinaryProtocol Handle)
              -> IO Bool
serverFunc a (h1,h2) = do
  let t1 = getTransport h1
  let t2 = getTransport h2
  
  putStrLn "Server go!"
  dat <- read_Work h1
  putStrLn "Recieved data:"
  print dat
  write_Work h1 testdata2
  tFlush t1
  putStrLn "Data written"

  return True

clientFunc :: HostName -> PortNumber -> IO ()
clientFunc host p = do
  putStrLn "Client go!"
  h <- connectTo host $ PortNumber p
  let proto = BinaryProtocol h
  write_Work proto testdata
  tFlush h
  putStrLn "Data sent 1, receiving."
  w <- read_Work proto
  putStrLn "Recieved 1:"
  print w
  write_Work proto testdata
  tFlush h
  putStrLn "Data sent 2, receiving."
  w <- read_Work proto
  putStrLn "Recieved 2:"
  print w
  tClose h
  
main :: IO ()
main = do
  a <- getArgs
  if elem "client" a then do clientFunc "127.0.0.1" port
    else do
    runBasicServer () serverFunc port
    putStrLn "Server stopped"