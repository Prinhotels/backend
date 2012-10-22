module GreetingClient where

import Network
import Greet_Types
import GreetingService_Client
import Thrift.Transport.Handle
import Thrift.Protocol.Binary

runclient = do
    handle <- hOpen("localhost", PortNumber 7911)
    let m = Message {f_Message_msg = Just " =DD " }
    result <- greet (BinaryProtocol handle, BinaryProtocol handle) m
    print result

