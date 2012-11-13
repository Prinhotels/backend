
import Control.Exception (bracket)
import System.IO (openFile, hClose,  IOMode(ReadMode), hGetContents)
import Data.List (elemIndex)
import Text.Regex (splitRegex, mkRegex)
import Text.Regex.PCRE.Light.Char8 (compile, match)
import Database.HDBC.ODBC (connectODBC, Connection)
import Database.HDBC (prepare, execute, toSql, Statement, quickQuery, fromSql)
import System.Environment (getArgs)
import Data.Digest.SHA1 (hash, Word160(..))
import Numeric (showHex)
import GHC.Enum (toEnum)
import Data.Bits ((.&.), shiftR)
import Data.Word (Word32, Word8)
import Codec.Binary.UTF8.String (encode)

main :: IO ()
main = do
    fmap head getArgs >>= process

process path = 
    bracket (openFile path ReadMode) hClose $ \h -> do
        c <- hGetContents h
        putStrLn $ sha1 c

sha1 :: String -> String
sha1 = concatMap toHex . expand . hash . encode
    where
        expand :: Word160 -> [Word8]
        expand (Word160 a b c d e) = concatMap w32toA8 [a, b, c, d, e]
        w32toA8 :: Word32 -> [Word8]
        w32toA8 w = [(fromIntegral (w `shiftR` s)) .&. (fromIntegral 15) | s <- [28,24..0]]
        toHex :: Word8 -> String
        toHex = flip (showHex . fromIntegral) ""