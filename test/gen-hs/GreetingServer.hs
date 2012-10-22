module GreetingServer where
import GreetingService_Iface
import GreetingService
import Greet_Types
import Thrift.Server
import Data.Maybe

data GREET = GREET
instance GreetingService_Iface GREET where
    greet GREET Nothing = do
    	print "Nothing"
    	return Message {f_Message_msg = Just "No argument"}
    greet GREET (Just m) = do
    	print "Just"
    	return m {f_Message_msg = Just ("Reply" ++ (fromJust (f_Message_msg m)))}

serve = runBasicServer GREET process 7911