
import Control.Exception (bracket)
import System.IO (openFile, hClose,  IOMode(ReadMode), hGetContents)
import Data.List (elemIndex, foldl')
import Text.Regex (splitRegex, mkRegex)
import Text.Regex.PCRE.Light.Char8 (compile, match)
import Database.HDBC.ODBC (connectODBC, Connection)
import Database.HDBC.MySQL(withRTSSignalsBlocked)
import Database.HDBC (prepare, execute, toSql, Statement, quickQuery, fromSql)
import System.Environment (getArgs)
import Data.Digest.SHA1 (hash, Word160(..))
import Numeric (showHex)
import Data.Bits ((.&.), shiftR)
import Data.Word (Word32, Word8)
import Codec.Binary.UTF8.String (encode)
import Data.Set (insert, empty, member, Set, singleton, toList, fromList, size)
import qualified Data.Set as DS (fold)
import Control.Monad (forM)

main :: IO ()
main = do
    db <- connectODBC $ "DRIVER={MySQL ODBC 5.2 Driver};unixSocket=/tmp/mysql.sock;" ++
                        "database=paxer_apache;user=paxer_brancher;password=@!?BV0ai$?#("
    stmt <- prepare db $ "INSERT INTO paxer_apache.access_log SET ip=?,keepalive=?,timestamp=?," ++
                         "status=?,seconds=?,miliseconds=?,response_size=?,protocol=?," ++
                         "url=?,referer=?,filename=?,user=?,cookie=?,navigator=?,hash=?"
    fmap head getArgs >>= process db stmt

process :: Connection -> Statement -> FilePath -> IO ()
process db ins path = do
    set <- bracket (openFile path ReadMode) hClose $ \h -> do
        content <- hGetContents h
        let findNextLine content s = case elemIndex '\n' content of
                Just n -> do
                    findNextLine (drop (n + 1) content) $ insert (take n content) s
                Nothing -> insert (sha1 content) s
        return $ findNextLine content empty
    red <- checkLines db set
    DS.fold (processLine db ins) (return ()) red

checkLines :: Connection -> Set String -> IO (Set String)
checkLines db s = do
    groups <- forM (DS.fold separarengrupos [] s) (reducirgrupo db)
    return $ fromList $ concat groups

separarengrupos :: String -> [Set String] -> [Set String]
separarengrupos s [] = [singleton s]
separarengrupos s l@(f:fs)
    | size f < 300000 = (insert s f):fs
    | otherwise = (singleton s):l

reducirgrupo :: Connection -> Set String -> IO [String]
reducirgrupo db s = withRTSSignalsBlocked $ do
    let w = "('" ++ (concatList (toList s) "','" (++ "')"))
    putStrLn w
    list <- quickQuery db ("SELECT hash FROM access_log WHERE hash IN " ++ w) []
    return $ fmap (fromSql . head) list

concatList :: [String] -> String -> (String -> String) -> String
concatList [a] _ l = l a
concatList (a:as) j l = concatList as j $ flip (++) (j ++ l a)


processLine :: Connection -> Statement -> String -> IO() -> IO ()
processLine db ins line _ = withRTSSignalsBlocked $
	case match (compile pattern []) line [] of
        Just (_:ip:keepalive:timestamp:code:secs:
              milisecs:size:protocol:url:referer:
              filename:user:cookie:browser:_) -> do
                execute ins [
                                toSql ip,
                                toSql keepalive,
                                toSql timestamp,
                                toSql code,
                                toSql secs,
                                toSql milisecs,
                                toSql size,
                                toSql protocol,
                                toSql url,
                                toSql referer,
                                toSql filename,
                                toSql user,
                                toSql cookie,
                                toSql browser,
                                toSql (sha1 line)
                            ]
                return ()
        _ -> putStrLn $ "no match: " ++ line
    where pattern = "^([^ ]+)" ++ --ip %h
                    " ([^ ]+)" ++ --keepalive seconds %k
                    " \\[([^\\]]+)\\]" ++ --timestamp %t
                    " ([^ ]+)" ++ -- response code %X%>s
                    " \\(([^ ]+)" ++ -- seconds since beggining of request (%T
                    " ([^ ]+)\\)" ++ -- miliseconds %D)
                    " ([^ ]+)" ++ -- response size in bytes %b
                    " ([^ ]+)" ++ -- protocol %H
                    " ([^ ]+)" ++ -- url %{Host}i%U%q
                    " ([^ ]+)" ++ -- referer %{Referer}i
                    " ([^ ]+)" ++ -- filename %f
                    " ([^ ]+)" ++ -- htdigest user %u
                    " ([^ ]+)" ++ -- cookie %{PHPSESSID}C
                    " (.+)" -- navigator %{User-Agent}i

sha1 :: String -> String
sha1 = concatMap toHex . expand . hash . encode
    where
        expand :: Word160 -> [Word8]
        expand (Word160 a b c d e) = concatMap w32toA8 [a, b, c, d, e]
        w32toA8 :: Word32 -> [Word8]
        w32toA8 w = [(fromIntegral (w `shiftR` s)) .&. (fromIntegral 15) | s <- [28,24..0]]
        toHex :: Word8 -> String
        toHex = flip (showHex . fromIntegral) ""