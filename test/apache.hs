
import Data.Int (Int64)
import Control.Parallel.Strategies (Strategy, parMap, using, NFData, rdeepseq, rseq)
import Control.Parallel (pseq)
import Control.DeepSeq (rnf)
import Control.Exception (bracket, finally)
import System.IO (openFile, hClose, Handle,  IOMode(ReadMode), hSeek, SeekMode(AbsoluteSeek), hFileSize, hIsEOF)
import Control.Monad (liftM, forM, forM_, mapM_)
import System.Environment (getArgs)

import Text.Regex.PCRE.Light (compile, match)
import Data.Foldable (foldl')
import Data.List.Ordered (sortBy)

import qualified Data.ByteString.Char8 as DBC
import qualified Data.ByteString.Lazy.Char8 as DBLC
import qualified Data.Map as DM

instance NFData DBC.ByteString -- import Data.ByteString.Lazy.Internal?


cTHREADS = 8

data ChunkSpec = CS {
    chunkOffset :: !Int64 , -- Estricto !
    chunkLength :: !Int64 -- Estricto !
} deriving (Eq , Show)

-- | 'mapReduce' es la técnica general que se compone de dos funciones: la que
--   convierte los chunks en datos y la que usa el arreglo de datos para generar
--   el dato final esperado. El procesamiento de chunks se hace en paralelo,
--   donde data_ contiene un arreglo de chunks (uno por hilo). Este procesamiento
--   debe finalizar para poder usar su valor en el folding que ocurre 
--   posteriormente en el hilo principal.
mapReduce :: Strategy b -- Para el map
        -> (a -> b) -- mapping
        -> Strategy c -- Para el fold
        -> ([b] -> c) -- folding
        -> [a] -- Lista original
        -> c -- Resultado
mapReduce mapS mapF redS redF data_ = mapRes `pseq` redRes
    where mapRes = parMap mapS mapF data_
          redRes = redF mapRes `using` redS

-- | 'withChunks' obtiene los chunks (perezosos) y sus handlers y se los pasa
--   a la función mapReduce para que se alimenten progresivamente del chunk
--   mientras se va leyendo. Hay un chunk por cada hilo disponible. Se espera
--   que el mapReduce devuelva el valor evaluado aprovechando el paralelismo.
--   Sin embargo se usa 'rnf' para garantizar un resultado completamente 
--   evaluado (no necesariamente en paralelo).
withChunks :: (NFData a) =>
              (FilePath -> IO [ChunkSpec])
           -> ([DBLC.ByteString] -> IO a)
           -> FilePath
           -> IO a
withChunks chunkFunc process path = do
    (chunks , handles) <- chunkedRead chunkFunc path
    r <- process chunks
    (rnf r `seq` return r) `finally` mapM_ hClose handles

-- | dada la función mapReduce y el archivo, genera la salida procesada por el
--   mapReduce (func)
chunkedReadWith :: (NFData a) =>
                   ([DBLC.ByteString] -> IO a)
                -> FilePath -> IO a
chunkedReadWith func path = withChunks (lineChunks cTHREADS)
    func path

-- | 'chunkedRead' abre el archivo tantas veces como chunks devuelva chunkFunc.
--   Cada chunk (perezoso hGetContents) se devuelve acompañado del handler.
chunkedRead :: (FilePath -> IO [ChunkSpec])
            -> FilePath
            -> IO ([DBLC.ByteString], [Handle])
chunkedRead chunkFunc path = do
    chunks <- chunkFunc path
    liftM unzip . forM chunks $ \ spec -> do
        h <- openFile path ReadMode
        hSeek h AbsoluteSeek (fromIntegral (chunkOffset spec))
        chunk <- DBLC.take ( chunkLength spec ) `liftM` DBLC.hGetContents h
        return (chunk , h)


-- | 'lineChunks' genera una lista perezosa de chunks. En este contexto nos
--   nos interesa un chunk por cada hilo. Esta tarea se ejecuta en el hilo
--   principal sin necesidad de usar paralelismo. (creo que es mentira)
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
                            bytes <- DBLC.hGet h 4096
                            case DBLC.elemIndex '\n' bytes of
                                Just n -> do
                                    chunks@ (c:_) <- findChunks (off + n + 1)
                                    let coff = chunkOffset c
                                    return (CS offset (coff - offset): chunks)
                                Nothing -> findNewline (off + DBLC.length bytes)
                findNewline newOffset
        findChunks 0
{-
lineCount :: [DBLC.ByteString] -> IO Int64
lineCount d = do
    putStrLn "HOLA"
    mapM_ (putStrLn . DBLC.unpack) d
    return $ mapReduce rdeepseq (DBLC.count '\n') rdeepseq sum d

main :: IO ()
main = do
    args <- getArgs
    forM_ args $ \ path -> do
        numLines <- chunkedReadWith lineCount path
        putStrLn $ path ++ ": " ++ show numLines
-}

countURLs :: [DBLC.ByteString] -> IO (DM.Map DBC.ByteString Int)
countURLs d = return $ mapReduce
    rseq (foldl' augment DM.empty . DBLC.lines)
    rseq (DM.unionsWith (+)) d
    where
        augment map line =
            case match ( compile pattern []) ( strict line ) [] of
                Just (_:url:_) -> DM.insertWith' (+) url 1 map
                _ -> map
        strict = DBC.concat . DBLC.toChunks
        pattern = DBC.pack $ "\"(?:GET|POST|HEAD) ([^ ]+) HTTP/"

main = do
    args <- getArgs
    forM_ args $ \ path -> do
        m <- chunkedReadWith countURLs path
        let mostPopular (_,a) (_,b) = compare b a
        mapM_ print . take 10 . sortBy mostPopular . DM.toList $ m