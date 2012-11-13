
import Control.Exception (bracket)
import System.IO (openFile, hClose,  IOMode(ReadMode), hGetContents, hSeek,  SeekMode(AbsoluteSeek))
import Data.List (elemIndex)
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

main :: IO ()
main = do
    db <- connectODBC $ "DRIVER={MySQL ODBC 5.2 Driver};unixSocket=/tmp/mysql.sock;" ++
                        "database=paxer_apache;user=paxer_brancher;password=@!?BV0ai$?#("
    stmt <- prepare db $ "INSERT INTO paxer_apache.access_log SET ip=?,keepalive=?,timestamp=?," ++
                         "status=?,seconds=?,miliseconds=?,response_size=?,protocol=?," ++
                         "url=?,referer=?,filename=?,user=?,cookie=?,navigator=?,pos=?"
    row <- withRTSSignalsBlocked $ quickQuery db ("SELECT pos FROM access_log ORDER BY id DESC LIMIT 1") []
    let start = if 0 == length row then 0 else (fromSql . head . head) row
    fmap head getArgs >>= processFile start (processLine stmt)

processFile :: Int -> (Int -> String -> IO ()) -> FilePath -> IO ()
processFile pos callback path = do
    bracket (openFile path ReadMode) hClose $ \h -> do
        hSeek h AbsoluteSeek (fromIntegral pos)
        content <- hGetContents h
        let findNextLine pos content = case elemIndex '\n' content of
                Just n -> do
                    callback (pos + n + 1) (take n content)
                    findNextLine (pos + n + 1) (drop (n + 1) content)
                Nothing -> do
                    callback (pos + (length content) + 1) content
        findNextLine pos content

processLine :: Statement -> Int -> String -> IO ()
processLine ins end line = withRTSSignalsBlocked $ do
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
                                toSql end
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