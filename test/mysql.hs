import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL

main = do
  rows <- do
    conn <- connectMySQL defaultMySQLConnectInfo {
              mysqlUnixSocket = "/tmp/mysql.sock",
              mysqlUser       = "paxer_brancher",
              mysqlPassword   = "@!?BV0ai$?#(",
              mysqlDatabase   = "prinh_haskell"
            }
    quickQuery' conn "SELECT 1 + 1" []
  forM_ rows $ \row -> putStrLn $ show row