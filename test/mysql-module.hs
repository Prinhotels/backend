
{-# OPTIONS_GHC -XMultiParamTypeClasses #-}

import Control.Monad
import Database.HDBC
import Database.HDBC.ODBC
import Database.HaskellDB
import Database.HaskellDB.HDBC.ODBC
import Database.HaskellDB.Sql.MySQL
import Database.HaskellDB.HDBRec

import Database.HDBC.SqlValue

import Database.Definition.TPR

import Data.Maybe
import Data.Time.Calendar
import Data.Time.LocalTime

-- *** Exception: Convertible: error converting source data SqlLocalTime 2012-04-27 10:22:15 of type SqlValue to type Data.Time.LocalTime.LocalTime.ZonedTime: incompatible types
-- import Data.Convertible.Base
-- instance Convertible SqlValue ZonedTime where
--     safeConvert SqlLocalTime t = utcToZonedTime t

main = odbcConnect generator opts $ \db -> do
    x <- query db $ table tPR
    mapM_ putStrLn (map (\r -> show $ toModifiedJulianDay $ localDay $ fromJust $ r!tpr_updated) x)
    update db tPR
         (\t -> t!tpr_id .==. (constant $ Just 1))
         (\t -> tpr_year << constant 1111)
    where opts = [("DRIVER", "{MySQL ODBC 5.2 Driver}"),
                  ("SERVER", "localhost"),
                  ("DATABASE", "prinh_haskell"),
                  ("UID", "paxer_brancher"),
                  ("PASSWORD", "@!?BV0ai$?#(")]

{-
main = mysqlConnect    opts $ \db -> do
    x <- query db simpleSelection
    putStrLn $ show $ fmap show x
    where opts = [("unixSocket", "/tmp/mysql.sock"),
                  ("database", "prinh_haskell"),
                  ("user", "paxer_brancher"),
                  ("password", "@!?BV0ai$?#(")]
          simpleSelection = table tPR
-}