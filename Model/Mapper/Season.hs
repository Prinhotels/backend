{-# LANGUAGE FlexibleContexts #-}

module Model.Mapper.Season (get, getByRange, seasonHasPlan, insert, update, updateM, delete) where

import Prelude

import Database.HDBC (run)
import Database.HDBC.ODBC (Connection)
import Database.HaskellDB ((!))
import Database.HaskellDB.Database (Database, query)
import qualified Database.HaskellDB.Database as DHD (insert, update, delete)
import Database.HaskellDB.HDBRec (Record, ShowRecRow, ShowLabels)
import Database.HaskellDB.Query ((.==.), (.<=.), (.>=.), (.<>.), (.&&.), table, project, (<<),
                                 restrict, constant, forUpdate, ToPrimExprs, InsertRec, Rel,
                                 count, constVal)

import Database.HDBC (quickQuery')
import Database.HDBC.SqlValue (SqlValue(SqlNull), fromSql, toSql)

import Data.Bits ((.&.),shiftR)
import Data.Word (Word8)
import Data.ByteString (pack, ByteString)
import Data.Maybe (listToMaybe, fromJust, maybe)
import Control.Monad.State (State, state)
import Data.List (foldl')
import Data.Char (intToDigit)

import Database.HaskellDB.Query (Table(Table))
import Database.HaskellDB.Sql.MySQL (generator)
import Database.HaskellDB.Sql.Generate (sqlUpdate)
import Database.HaskellDB.Sql.Print (ppUpdate)

import qualified Data.Set as DS
import qualified Data.Map as DM
import qualified Model.Database.Definition.TPR as TPR
import qualified Model.Database.Definition.VIEW_TPR as VTPR

{- TODO implementar procesos adicionales del mapper abstract -}

import Numeric (showIntAtBase)

-- | La clave es el la tabla y el valor el conjunto de registros bloqueados
type DBLocks = DM.Map String (DS.Set Int)


-- | Agrega registros al conjunto de registros bloqueados de una tabla
lock :: String -> [Int] -> DBLocks -> DBLocks
lock tableName = DM.insertWith DS.union tableName . DS.fromList


-- | Obtiene la id de un registro de temporadas. El fromJust viene de que
--   el id es nulleable al insertar, pero al leer siempre trae un valor.
rid :: Record VTPR.VIEW_TPR_result -> Int
rid = fromJust . flip (!) VTPR.vtpr_id


get :: Database -> Int -> IO (State DBLocks (Maybe (Record VTPR.VIEW_TPR_result)))
get = get_ False


getWithExclusiveLock :: Database -> Int -> IO (State DBLocks (Maybe (Record VTPR.VIEW_TPR_result)))
getWithExclusiveLock = get_ True


get_ :: Bool -> Database -> Int -> IO (State DBLocks (Maybe (Record VTPR.VIEW_TPR_result)))
get_ fu db tpr_id = do
    mrow <- fmap listToMaybe $ query db $ do
        t <- table VTPR.vIEW_TPR
        restrict $ (t!VTPR.vtpr_id) .==. (constant (Just tpr_id))
        if fu then forUpdate else return ()
        return t
    return . state $ \locks -> (mrow, nlocks locks mrow)
    where
        nlocks :: DBLocks -> Maybe (Record VTPR.VIEW_TPR_result) -> DBLocks
        nlocks locks = maybe locks (\ row -> lock "TPR" [rid row] locks)


getByRange :: Database -> Int -> Int -> Int -> IO (State DBLocks [Record VTPR.VIEW_TPR_result])
getByRange = getByRange_ False


getByRangeWithExclusiveLock :: Database -> Int -> Int -> Int -> IO (State DBLocks [Record VTPR.VIEW_TPR_result])
getByRangeWithExclusiveLock = getByRange_ True


getByRange_ :: Bool -> Database -> Int -> Int -> Int -> IO (State DBLocks [Record VTPR.VIEW_TPR_result])
getByRange_ fu db hot_id yearFrom yearTo = do
    rows <- query db $ do
        t <- table VTPR.vIEW_TPR
        restrict $ (t!VTPR.vtpr_hot_id) .==. (constant hot_id)
              .&&. (t!VTPR.vtpr_year) .>=. (constant yearFrom)
              .&&. (t!VTPR.vtpr_year) .<=. (constant yearTo)
              .&&. (t!VTPR.vtpr_status_reg) .<>. (constant "deleted")
        if fu then forUpdate else return ()
        return t
    return . state $ \locks -> (rows, lock "TPR" (map rid rows) locks)


seasonHasPlan :: Connection -> Int -> Int -> Int -> IO (State DBLocks (DM.Map Int Bool))
seasonHasPlan rawconn tpr_hot_id tpr_year_from tpr_year_to = do
    rows <- quickQuery' rawconn query [toSql tpr_hot_id,
                                       toSql tpr_year_from,
                                       toSql tpr_year_to]
    return . state $ \locks -> (foldl' (flip maprow) DM.empty rows, locks)
    where
        query = "SELECT tpr_id, ptp_active FROM TPR " ++
                  "JOIN LEFT PTP ON tpr_id = ptp_tpr_id " ++
                  "WHERE tpr_hot_id = ? AND ? <= tpr_year AND tpr_year <= ? " ++
                  "tpr_status_reg='active' " ++
                  "ORDER BY tpr_id"
        maprow [tpr_id_s, ptp_active_s] = DM.insertWith (||) tpr_id hasPlan
            where
                tpr_id = fromSql tpr_id_s
                hasPlan = SqlNull /= ptp_active_s && (fromSql ptp_active_s :: Int) /= 0

seasonExists :: Database -> Int -> String -> IO Bool
seasonExists db hot_id tpr_name = do
    fmap ((/=) 0 . flip (!) TPR.tpr_updated_by . head) $ query db $ do
        t <- table TPR.tPR
        restrict $ (t!TPR.tpr_hot_id) .==. (constant hot_id)
              .&&. (t!TPR.tpr_name) .==. (constant tpr_name)
              .&&. (t!TPR.tpr_status_reg) .<>. (constant "deleted")
        -- se reescribe la columna tpr_updated_by. esto es una construccion, se asume agrupamiento automatico en mysql
        project (TPR.tpr_updated_by << count (t!TPR.tpr_id))
        return t


insert :: (ShowRecRow r, ToPrimExprs r, InsertRec r TPR.TPR) =>
           Database -> Connection -> Record r -> IO Int
insert db rawconn rcrd = do
    DHD.insert db TPR.tPR rcrd
    fmap (fromSql . head . head) $ quickQuery' rawconn "SELECT LAST_INSERT_ID();" []

{- TODO validar bloqueo de registros -}
update :: (ShowLabels s, ToPrimExprs s) =>
          Database -> Connection -> Int -> (Rel TPR.TPR -> Record s) -> IO Int
update db rawconn id rcrd = do
    --putStrLn $ show $ ppUpdate $ sqlUpdate generator ((\(Table name _) -> name) TPR.tPR) (\t -> t!TPR.tpr_id .==. constVal id) rcrd
    DHD.update db TPR.tPR (\t -> t!TPR.tpr_id .==. constVal id) rcrd
    fmap (fromSql . head . head) $ quickQuery' rawconn "SELECT ROW_COUNT();" []

tb :: Int -> ByteString
tb d = pack [a d 24,a d 16,a d 8,a d 0]
  where a :: Int -> Int -> Word8
        a d = fromIntegral . (255 .&.) . shiftR d

{- TODO validar bloqueo de registros -}
updateM :: Database -> Connection -> Int -> [Int] -> IO Int
updateM db rawconn id rcrd = do
    run rawconn q $ maph id rcrd
    putStrLn $ show . head $ rcrd
    fmap (fromSql . head . head) $ quickQuery' rawconn "SELECT ROW_COUNT();" []
    where maph :: Int -> [Int] -> [SqlValue]
          maph i []     = [toSql i]
          maph i (x:xs) = (toSql . tb) x : maph i xs
          q = "UPDATE TPR SET tpr_data_5 = ?, tpr_data_6 = ?, tpr_data_7 = ?, tpr_data_8 = ?"
                        ++ ", tpr_data_9 = ?, tpr_data_10 = ?, tpr_data_11 = ?, tpr_data_12 = ?"
                        ++ ", tpr_data_1 = ?, tpr_data_2 = ?, tpr_data_3 = ?, tpr_data_4 = ?"
                         ++ " WHERE tpr_id = ?"


{- TODO validar bloqueo de registros -}
delete :: Database -> Connection -> Int -> IO Int
delete db rawconn id = do
    DHD.delete db TPR.tPR (\t -> t!TPR.tpr_id .==. constVal id)
    fmap (fromSql . head . head) $ quickQuery' rawconn "SELECT ROW_COUNT();" []
