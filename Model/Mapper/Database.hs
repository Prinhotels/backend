
module Model.Mapper.Database (irawconn, describe) where

import Prelude

import Database.HaskellDB (Database)
import Database.HaskellDB (describe)
--import Database.HaskellDB.HDBC.ODBC (odbcConnect)
import Database.HaskellDB.Sql.MySQL (generator)
import Database.HaskellDB.HDBC (hdbcConnect)
import Database.HDBC.ODBC (connectODBC, Connection)


{-
dbconn :: (Database -> IO a) -> IO a
dbconn = odbcConnect generator [("DRIVER", "{MySQL ODBC 5.2 Driver}"),
                                ("server", "localhost"),
                                ("database", "prinh_haskell"),
                                ("uid", "paxer_brancher"),
                                ("password", "@!?BV0ai$?#(")]
-}

irawconn :: IO Connection
irawconn = connectODBC $ "DRIVER={MySQL ODBC 5.2 Driver};" ++
                         "server=localhost; " ++
                         "database=prinh_haskell; " ++
                         "uid=paxer_brancher; " ++
                         "password=@!?BV0ai$?#("

--hdbconn :: (Database -> IO a) -> IO a
--hdbconn = hdbcConnect generator irawconn
