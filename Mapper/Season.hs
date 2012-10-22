module Mapper.Season (get, getByRange) where

import Database.HaskellDB.PrintQuery (ppSql)

import Data.Maybe (listToMaybe, fromJust, maybe)
import Database.HaskellDB ((!))
import Database.HaskellDB.Database (Database, query)
import Database.HaskellDB.HDBRec (Record, RecCons)
import Database.HaskellDB.Query ((.==.), (.<=.), (.>=.), (.<>.), (.&&.), table, restrict, constant, forUpdate)
import Control.Monad.State (State, state)

import qualified Data.Set as DS
import qualified Data.Map as DM
import qualified Database.Definition.TPR as TPR

-- | La clave es el la tabla y el valor el conjunto de registros bloqueados
type DBLocks = DM.Map String (DS.Set Int)

-- | Agrega registros al conjunto de registros bloqueados de una tabla
lock :: String -> [Int] -> DBLocks -> DBLocks
lock tableName = DM.insertWith DS.union tableName . DS.fromList

-- | Obtiene la id de un registro de temporadas. El fromJust viene de que
--   el id es nulleable al insertar, pero al leer siempre trae un valor.
rid :: Record TPR.TPR_result -> Int
rid = fromJust . flip (!) TPR.tpr_id


get :: Database -> Int -> IO (State DBLocks (Maybe (Record TPR.TPR_result)))
get db tpr_id = do
    mrow <- fmap listToMaybe $ query db $ do
        t <- table TPR.tPR
        restrict $ (t!TPR.tpr_id) .==. (constant (Just tpr_id))
        return t
    return . state $ \locks -> (mrow, nlocks locks mrow)
    where
        nlocks :: DBLocks -> Maybe (Record TPR.TPR_result) -> DBLocks
        nlocks locks = maybe locks (\ row -> lock "TPR" [rid row] locks)


getByRange :: Database -> Int -> Int -> Int -> IO (State DBLocks [Record TPR.TPR_result])
getByRange db hot_id yearFrom yearTo = do
    putStrLn $ show $ ppSql $ do
        t <- table TPR.tPR
        restrict $ (t!TPR.tpr_hot_id) .==. (constant hot_id)
              .&&. (t!TPR.tpr_year) .>=. (constant yearFrom)
              .&&. (t!TPR.tpr_year) .<=. (constant yearTo)
              .&&. (t!TPR.tpr_status_reg) .<>. (constant "deleted")
        forUpdate
        return t
    rows <- query db $ do
        t <- table TPR.tPR
        restrict $ (t!TPR.tpr_hot_id) .==. (constant hot_id)
              .&&. (t!TPR.tpr_year) .>=. (constant yearFrom)
              .&&. (t!TPR.tpr_year) .<=. (constant yearTo)
              .&&. (t!TPR.tpr_status_reg) .<>. (constant "deleted")
        forUpdate
        return t
    return . state $ \locks -> (rows, lock "TPR" (map rid rows) locks)