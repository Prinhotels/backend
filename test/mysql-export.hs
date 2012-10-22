
import Database.HaskellDB.Sql.MySQL
import Database.HaskellDB.DBDirect
import Database.HaskellDB.DriverAPI

import Database.HDBC.ODBC
import Database.HaskellDB.HDBC.ODBC
import Database.HDBC.MySQL(withRTSSignalsBlocked)

-- cabal install --force-reinstalls && cd driver-hdbc && cabal install --reinstall --force-reinstalls && cd ../driver-hdbc-odbc && cabal install --reinstall && cd ..
-- cd test && ghc --make -fforce-recomp -threaded mysql-export.hs && cd .. && test/mysql-export Database.Definition prinh_haskell 'DRIVER={MySQL ODBC 5.2 Driver},SERVER=localhost,DATABASE=prinh_haskell,UID=paxer_brancher,PASSWORD=@!?BV0ai$?#('

main = withRTSSignalsBlocked $ dbdirect defaultdriver {connect = odbcConnect generator}

{-

import Database.HDBC.MySQL
import Database.HaskellDB.HDBC.MySQL

-- ./mysql-export Database.Definition prinh_haskell 'unixSocket=/tmp/mysql.sock,database=prinh_haskell,user=paxer_brancher,password=@!?BV0ai$?#('

main = dbdirect defaultdriver {connect = mysqlConnect generator}
-}