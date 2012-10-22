{-

import Database.HaskellDB.PrintQuery (ppSql)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans (liftIO, lift)

getByRange :: Int -> Int -> Int -> ReaderT Database IO (State String [Record TPR.TPR_result])
getByRange hot_id yearFrom yearTo = do
    db <- ask
    liftIO $ do
        putStrLn "getByRange"
        putStrLn $ show $ ppSql $ do
            t <- table TPR.tPR
            restrict $ (t!TPR.tpr_hot_id) .==. (constant hot_id)
                  .&&. (t!TPR.tpr_year) .>=. (constant yearFrom)
                  .&&. (t!TPR.tpr_year) .<=. (constant yearTo)
                  .&&. (t!TPR.tpr_status_reg) .<>. (constant "deleted")
            return t
        x <- query db $ do
            t <- table TPR.tPR
            restrict $ (t!TPR.tpr_hot_id) .==. (constant hot_id)
                  .&&. (t!TPR.tpr_year) .>=. (constant yearFrom)
                  .&&. (t!TPR.tpr_year) .<=. (constant yearTo)
                  .&&. (t!TPR.tpr_status_reg) .<>. (constant "deleted")
            return t
        return $ do
            put "hola"
            return x

getByRange :: Int -> Int -> Int -> ReaderT Database IO (State String [Record TPR.TPR_result])
getByRange hot_id yearFrom yearTo = do
    db <- ask
    liftIO $ do
        x <- query db $ do
            t <- table TPR.tPR
            restrict $ (t!TPR.tpr_hot_id) .==. (constant hot_id)
                  .&&. (t!TPR.tpr_year) .<=. (constant yearFrom)
                  .&&. (t!TPR.tpr_year) .>=. (constant yearTo)
                  .&&. (t!TPR.tpr_status_reg) .<>. (constant "deleted")
            return t
        return $ do
            put "hola"
            return x

getByRange :: Int -> Int -> Int -> ReaderT Database (StateT String IO) [Record TPR.TPR_result]
getByRange hot_id yearFrom yearTo = do
    db <- ask
    put "hola"
    liftIO $ query db $ do
        t <- table TPR.tPR
        restrict $ (t!TPR.tpr_hot_id) .==. (constant hot_id)
              .&&. (t!TPR.tpr_year) .<=. (constant yearFrom)
              .&&. (t!TPR.tpr_year) .>=. (constant yearTo)
              .&&. (t!TPR.tpr_status_reg) .<>. (constant "deleted")
        return t

getByRange :: Int -> Int -> Int -> ReaderT Database IO [Record TPR.TPR_result]
getByRange hot_id yearFrom yearTo = do
    db <- ask
    liftIO $ query db $ do
        t <- table TPR.tPR
        restrict $ (t!TPR.tpr_hot_id) .==. (constant hot_id)
              .&&. (t!TPR.tpr_year) .<=. (constant yearFrom)
              .&&. (t!TPR.tpr_year) .>=. (constant yearTo)
              .&&. (t!TPR.tpr_status_reg) .<>. (constant "deleted")
        return t

        $hot_id   = (int) $hot_id;
        $yearFrom = (int) $yearFrom;
        $yearTo   = (int) $yearTo;

        list($TPR) = $this->_tableInfo('TPR');

        // Se buscan las temporadas que pertenezcan a esos años
        $select = $TPR->select(Zend_Db_Table::SELECT_WITH_FROM_PART)
            ->where("tpr_year BETWEEN $yearFrom AND $yearTo AND tpr_hot_id = $hot_id AND tpr_status_reg<>'deleted'")
            ->order('tpr_id');
        $rowSet = $this->_db->fetchAll($this->_db->lockSelect($select, $locking));
        // Se indica al DbTable del bloqueo de los registros
        if ($this->_db->inTransaction() && $locking == ZendP_Db_Adapter_Mysqli::FOR_UPDATE) {
            foreach ($rowSet as $row) {
                $TPR->lock($row['tpr_id']);
            }
        }

        // Se indexan las temporadas conseguidas por sus id's
        $result = array();
        foreach ($rowSet as $row) {
            $result[$row['tpr_id']] = $row;
        }

        return $result;
-}