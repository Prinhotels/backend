
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (State, runState, put)
import Control.Monad.Trans (liftIO)


import Database.HDBC.MySQL(withRTSSignalsBlocked)
import Database.HaskellDB ((!))
import Database.HaskellDB.Database (Database, query)
import Database.HaskellDB.Query ((.==.), (.<=.), (.>=.), (.<>.), (.&&.), table, restrict, constant)
import Database.HaskellDB.HDBRec (Record)
import Mapper.Database (dbconn)

import qualified Database.Definition.TPR as TPR

main :: IO ()
main = withRTSSignalsBlocked $ dbconn $ \db -> do
    stater <- runReaderT reader db
    putStrLn "got stater"
    let (rec, recRequested) = runState stater False
    putStrLn $ "rec given is " ++ show rec

reader :: ReaderT Database IO (State Bool (Record TPR.TPR_result))
reader = do
    db <- ask
    liftIO $ do
        putStrLn $ "got in"
        content <- query db $ table TPR.tPR
        return . return $ head content