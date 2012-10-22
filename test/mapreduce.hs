
import Data.Int (Int64)
import Control.Parallel.Strategies (Strategy, parMap, using, NFData, rdeepseq)
import Control.Parallel (pseq)
import Control.DeepSeq (rnf)
import Control.Exception (bracket, finally)
import System.IO (openFile, hClose, Handle,  IOMode(ReadMode), hSeek, SeekMode(AbsoluteSeek), hFileSize, hIsEOF)
import Control.Monad (liftM, forM, forM_)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Lazy.Char8 as DBLC

cTHREADS = 8
nlf = (head . DBL.unpack . DBLC.singleton) '\n'

data ChunkSpec = CS {
    chunkOffset :: !Int64 , -- Estricto !
    chunkLength :: !Int64 -- Estricto !
} deriving (Eq , Show)

mapReduce :: Strategy b -- Para el map
        -> (a -> b) -- mapping
        -> Strategy c -- Para el fold
        -> ([b] -> c) -- folding
        -> [a] -- Lista original
        -> c -- Resultado
mapReduce mapS mapF redS redF data_ = mapRes `pseq` redRes
    where mapRes = parMap mapS mapF data_
          redRes = redF mapRes `using` redS

withChunks :: (NFData a) =>
              (FilePath -> IO [ChunkSpec])
           -> ([DBL.ByteString] -> a)
           -> FilePath
           -> IO a
withChunks chunkFunc process path = do
    (chunks , handles) <- chunkedRead chunkFunc path
    let r = process chunks
    (rnf r `seq` return r) `finally` mapM_ hClose handles

chunkedReadWith :: (NFData a) =>
                   ([DBL.ByteString] -> a)
                -> FilePath -> IO a
chunkedReadWith func path = withChunks (lineChunks cTHREADS)
    func path

chunkedRead :: (FilePath -> IO [ChunkSpec])
            -> FilePath
            -> IO ([DBL.ByteString], [Handle])
chunkedRead chunkFunc path = do
    chunks <- chunkFunc path
    liftM unzip . forM chunks $ \ spec -> do
        h <- openFile path ReadMode
        hSeek h AbsoluteSeek (fromIntegral (chunkOffset spec))
        chunk <- DBL.take ( chunkLength spec ) `liftM` DBL.hGetContents h
        return (chunk , h)

lineChunks :: Int -> FilePath -> IO [ChunkSpec]
lineChunks numChunks path = do
    bracket (openFile path ReadMode) hClose $ \h -> do
        totalSize <- fromIntegral `liftM` hFileSize h
        let chunkSize = totalSize `div` fromIntegral numChunks
            findChunks offset = do
                let newOffset = offset + chunkSize
                hSeek h AbsoluteSeek $ fromIntegral newOffset
                -- falta encontrar el '\n' más cercano
                let findNewline off = do
                    eof <- hIsEOF h
                    if eof
                        then return [CS offset (totalSize - offset)]
                        else do
                            bytes <- DBL.hGet h 4096
                            case DBL.elemIndex nlf bytes of
                                Just n -> do
                                    chunks@ (c:_) <- findChunks (off + n + 1)
                                    let coff = chunkOffset c
                                    return (CS offset (coff - offset): chunks)
                                Nothing -> findNewline (off + DBL.length bytes)
                findNewline newOffset
        findChunks 0

lineCount :: [DBL.ByteString] -> Int64
lineCount = mapReduce rdeepseq (DBL.count nlf)
                      rdeepseq sum
main :: IO ()
main = do
    args <- getArgs
    forM_ args $ \ path -> do
        numLines <- chunkedReadWith lineCount path
        putStrLn $ path ++ ": " ++ show numLines