


--module Main where

--import Data.List
import System.IO (print, Handle)
--import Network
--import System.Environment (getArgs)

-- Thrift libraries
--import Thrift
import Thrift.Transport.Handle (tFlush)
import Thrift.Protocol (getTransport)
import Thrift.Protocol.Binary (BinaryProtocol)
import Thrift.Server (runBasicServer)

-- Generated Thrift modules
--import Test_Types




-- import Mapper.Database (describeTable)
import Mapper.Database (dbconn, describe, rawconn)
import Mapper.Season (get, getByRange)

import Data.Maybe (fromJust)
import Database.HaskellDB ((!))
import Database.HDBC.MySQL(withRTSSignalsBlocked)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runState)

import qualified Data.Map as DM
import Database.Definition.TPR
import TPR_Types (read_TPR, write_TPR, TPR(f_TPR_tpr_id, f_TPR_tpr_year))

-- ghc --make -fforce-recomp -threaded Main.hs && ./Main

--main = withRTSSignalsBlocked $ dbconn $ \db -> do
	--x <- get db 1
	--putStrLn $ show $ (fromJust x)!tpr_id
	--y <- describe db "TPR"
	--putStrLn $ show y
	--putStrLn $ describeTable rawconn "TPR"


-- Functions

serverFunc :: a -> (BinaryProtocol Handle, BinaryProtocol Handle)
              -> IO Bool
serverFunc a (h1,h2) = do
	let t1 = getTransport h1
	let t2 = getTransport h2

	putStrLn "Server go!"
	dat <- read_TPR h1
	putStrLn "Recieved data:"
	print dat
	withRTSSignalsBlocked $ dbconn $ \db -> do
		stater <- getByRange db 1 1000 3000
		putStrLn "Got stater"
		let (a, _) = runState stater DM.empty --(fromIntegral $ fromJust $ f_TPR_tpr_id dat)
		putStrLn "Got list"
		putStrLn $ show $ a
		let x = head a
		putStrLn $ show $ (x)!tpr_year
		write_TPR h1 dat {f_TPR_tpr_year = Just (fromIntegral $ (x)!tpr_year)}
		tFlush t1
		putStrLn "Data written"
		putStrLn $ show $ (x)!tpr_year

	return False

main :: IO ()
main = do
    runBasicServer () serverFunc 4390
    putStrLn "Server stopped"

{-  
main :: IO ()
main = withRTSSignalsBlocked $ dbconn $ \db -> do
		x <- getByRange db 1 2 3
		return ()
-}