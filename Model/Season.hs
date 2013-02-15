
module Model.Season (Tpr, get, GetInfo, getInfo, Get, update, Update, set, Set, insert, Insert) where {--}

import Prelude
import Data.Time.Calendar (Day(ModifiedJulianDay), toGregorian, fromGregorianValid, toModifiedJulianDay, fromGregorian)
import qualified Data.IntMap as DI
import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.Aeson.Types (ToJSON, toJSON, (.=), (.:), (.:?), object, Value(Object, Null, String), Pair, FromJSON(parseJSON))
import Control.Applicative ((<*>), (<$>), pure)
import Control.Monad (MonadPlus(mzero))
import Control.Monad.State (runState)
import Data.Bits ((.&.),(.|.),complement)
import Data.Bool.HT (if')
import Data.List (foldl')
import Data.Text (unpack, Text)
import Data.Maybe (fromJust, maybe, isJust)
import Database.HDBC.ODBC (Connection)
import Database.HDBC.MySQL(withRTSSignalsBlocked)
import Database.HaskellDB.HDBRec ((#))
import Database.HaskellDB.Query ((<<-), (<<), func, Expr, exprs, constant, constNull)
import Database.HaskellDB (Database)
import Database.HaskellDB ((!))
import Database.HaskellDB.HDBRec (Record, ShowLabels)
import Database.HaskellDB.Database (Database(dbDescribe))

-- import Data.Time.Format () --instance Read Day
-- import Data.Bits (shiftR)
-- import Data.Char (ord)
-- import Debug.Trace (trace)
-- import Data.Word (Word8)

import qualified Model.Database.Definition.VIEW_TPR as VTPR
import qualified Model.Database.Definition.TPR as TPR
import qualified Model.Mapper.Season as MMSeason

data TprFull = TprFull {
    ftpr_id :: Maybe Int,
    ftpr_name :: String,
    ftpr_year :: Int,
    ftpr_description :: String,
    ftpr_hot_id :: Int,
    ftpr_restriction :: String,
    ftpr_min_nights :: Maybe Int,
    ftpr_status_reg :: String,
    ftpr_updated_by :: Int,
    ftpr_data_5 :: Int,
    ftpr_data_6 :: Int,
    ftpr_data_7 :: Int,
    ftpr_data_8 :: Int,
    ftpr_data_9 :: Int,
    ftpr_data_10 :: Int,
    ftpr_data_11 :: Int,
    ftpr_data_12 :: Int,
    ftpr_data_1 :: Int,
    ftpr_data_2 :: Int,
    ftpr_data_3 :: Int,
    ftpr_data_4 :: Int
} deriving (Show, Eq, Ord)

instance ToJSON TprFull where
    toJSON t = object [
        "tpr_id" `ad` (ftpr_id t),
        "tpr_name" .= ftpr_name t,
        "tpr_year" .= ftpr_year t,
        "tpr_description" .= ftpr_description t,
        "tpr_hot_id" .= ftpr_hot_id t,
        "tpr_restriction" .= ftpr_restriction t,
        "tpr_min_nights" .= ftpr_min_nights t,
        "tpr_status_reg" .= ftpr_status_reg t,
        "tpr_updated_by" .= ftpr_updated_by t,
        "tpr_data_5" .= ftpr_data_5 t,
        "tpr_data_6" .= ftpr_data_6 t,
        "tpr_data_7" .= ftpr_data_7 t,
        "tpr_data_8" .= ftpr_data_8 t,
        "tpr_data_9" .= ftpr_data_9 t,
        "tpr_data_10" .= ftpr_data_10 t,
        "tpr_data_11" .= ftpr_data_11 t,
        "tpr_data_12" .= ftpr_data_12 t,
        "tpr_data_1" .= ftpr_data_1 t,
        "tpr_data_2" .= ftpr_data_2 t,
        "tpr_data_3" .= ftpr_data_3 t,
        "tpr_data_4" .= ftpr_data_4 t
     ]

data Tpr = Tpr {
    tpr_id :: Maybe Int,
    tpr_name :: String,
    tpr_year :: Int,
    tpr_description :: String,
    tpr_hot_id :: Int,
    tpr_restriction :: String,
    tpr_min_nights :: Maybe Int,
    tpr_status_reg :: String,
    tpr_updated_by :: Int
} deriving (Show)

instance ToJSON Tpr where
    toJSON t = object [
        "tpr_id" `ad` (tpr_id t),
        "tpr_name" .= tpr_name t,
        "tpr_year" .= tpr_year t,
        "tpr_description" .= tpr_description t,
        "tpr_hot_id" .= tpr_hot_id t,
        "tpr_restriction" .= tpr_restriction t,
        "tpr_min_nights" .= tpr_min_nights t,
        "tpr_status_reg" .= tpr_status_reg t,
        "tpr_updated_by" .= tpr_updated_by t
     ]

instance FromJSON Tpr where
    parseJSON (Object v) = Tpr <$>
                           v .: "tpr_id" <*>
                           v .: "tpr_name" <*>
                           v .: "tpr_year" <*>
                           v .: "tpr_description" <*>
                           v .: "tpr_hot_id" <*>
                           v .: "tpr_restriction" <*>
                           v .: "tpr_min_nights" <*>
                           v .: "tpr_status_reg" <*>
                           v .: "tpr_updated_by"

data TprPartial = TprPartial {
    ptpr_id :: Maybe (Maybe Int),
    ptpr_name :: Maybe String,
    ptpr_year :: Maybe Int,
    ptpr_description :: Maybe String,
    ptpr_hot_id :: Maybe Int,
    ptpr_restriction :: Maybe String,
    ptpr_min_nights :: Maybe (Maybe Int),
    ptpr_status_reg :: Maybe String,
    ptpr_updated_by :: Maybe Int
} deriving (Show)

instance FromJSON TprPartial where
    parseJSON (Object v) = TprPartial <$>
                           v .:? "tpr_id" <*>
                           v .:? "tpr_name" <*>
                           v .:? "tpr_year" <*>
                           v .:? "tpr_description" <*>
                           v .:? "tpr_hot_id" <*>
                           v .:? "tpr_restriction" <*>
                           v .:? "tpr_min_nights" <*>
                           v .:? "tpr_status_reg" <*>
                           v .:? "tpr_updated_by"


instance FromJSON Day where
    parseJSON (String s) = ((\((d, _):_) -> pure d) . reads . unpack) s


ad :: (ToJSON a) => Text -> Maybe a -> Pair
ad l = (,) l . maybe Null toJSON

jd :: Int -> Day
jd = ModifiedJulianDay . fromIntegral

dateToInt :: Integer -> Int -> Int -> Maybe Int
dateToInt y m = fmap dayToInt . (fromGregorianValid y m)

dayToInt :: Day -> Int
dayToInt = fromIntegral . toModifiedJulianDay

data Get = Get Int Day Day
instance FromJSON Get where
    parseJSON (Object v) = Get <$>
                           v .: "hot_id" <*>
                           v .: "dateFrom" <*>
                           v .: "dateTo"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

get :: Connection -> Database -> Get -> IO (DI.IntMap Int)
get _ db (Get hot_id jDayFrom jDayTo) = do
    withRTSSignalsBlocked $ do
        locker <- MMSeason.getByRange db hot_id (toRowYear jDayFrom) (toRowYear jDayTo)
        let (rows, _) = runState locker DM.empty
        return $ DI.filterWithKey validDay $ foldl' mergeTprYear DI.empty rows
    where
        validDay :: DI.Key -> Int -> Bool
        validDay k _ = (dayToInt jDayFrom) <= k && k <= (dayToInt jDayTo)
        -- obtener el anyo
        toRowYear :: Day -> Int
        toRowYear = (\(y, m, _) -> (fromIntegral y) - if m < 5 then 1 else 0) . toGregorian
        -- unir un anyo para una temporada en particular
        mergeTprYear :: DI.IntMap Int -> Record VTPR.VIEW_TPR_result -> DI.IntMap Int
        mergeTprYear a r = (\(_, a) -> a) $
            foldl' (mergeTprMonth (fromJust $ r!VTPR.vtpr_id) (r!VTPR.vtpr_year)) (5, a)
                [r!VTPR.vtpr_data_5, r!VTPR.vtpr_data_6,  r!VTPR.vtpr_data_7,
                 r!VTPR.vtpr_data_8, r!VTPR.vtpr_data_9,  r!VTPR.vtpr_data_10,
                 r!VTPR.vtpr_data_11, r!VTPR.vtpr_data_12, r!VTPR.vtpr_data_1,
                 r!VTPR.vtpr_data_2,  r!VTPR.vtpr_data_3,  r!VTPR.vtpr_data_4]
        -- incluir la aplicacion de un mes para una temporada en particular
        mergeTprMonth :: Int -> Int -> (Int, DI.IntMap Int) -> Int -> (Int, DI.IntMap Int)
        mergeTprMonth i tpr_year (m, a) c = (next, mergeTprDay 0 a)
          where next = if 12 == m then 1 else m + 1
                y = fromIntegral (tpr_year + if m < 5 then 1 else 0)
                mergeTprDay :: Int -> DI.IntMap Int -> DI.IntMap Int
                mergeTprDay 32 a = a
                mergeTprDay d a = mergeTprDay (d + 1) newa
                  where newa = maybe a aux (dateToInt y m d)
                        aux day = DI.insertWith (if' b) day (if' b i 0) a
                        b = 2 ^ (31 - d) .&. c == 2 ^ (31 - d)


data GetInfo = GetInfo Int Day Day
instance FromJSON GetInfo where
    parseJSON (Object v) = GetInfo <$>
                           v .: "hot_id" <*>
                           v .: "dateFrom" <*>
                           v .: "dateTo"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

toYear :: Day -> Int
toYear = (\(y, m, _) -> (fromIntegral y) - if m < 5 then 1 else 0) . toGregorian


getInfo :: Connection -> Database -> GetInfo -> IO [Tpr]
getInfo _ db (GetInfo hot_id jDayFrom jDayTo) = do
    (fmap . fmap) c $ getInfo_ db hot_id jDayFrom jDayTo
    where c t = Tpr {
                    tpr_id = ftpr_id t,
                    tpr_name = ftpr_name t,
                    tpr_year = ftpr_year t,
                    tpr_description = ftpr_description t,
                    tpr_hot_id = ftpr_hot_id t,
                    tpr_restriction = ftpr_restriction t,
                    tpr_min_nights = ftpr_min_nights t,
                    tpr_status_reg = ftpr_status_reg t,
                    tpr_updated_by = ftpr_updated_by t
                }


getInfo_ :: Database -> Int -> Day -> Day -> IO [TprFull]
getInfo_ db hot_id jDayFrom jDayTo = do
    withRTSSignalsBlocked $ do
        locker <- MMSeason.getByRange db hot_id (toYear jDayFrom) (toYear jDayTo)
        let (rows, _) = runState locker DM.empty
        return $ fmap extractR rows
    where
        -- toYear = (read . take 4 . show) -- 40587
        extractR r = TprFull {
            ftpr_id = r!VTPR.vtpr_id,
            ftpr_name = r!VTPR.vtpr_name,
            ftpr_year = r!VTPR.vtpr_year,
            ftpr_description = r!VTPR.vtpr_description,
            ftpr_hot_id = r!VTPR.vtpr_hot_id,
            ftpr_restriction = r!VTPR.vtpr_restriction,
            ftpr_min_nights = r!VTPR.vtpr_min_nights,
            ftpr_status_reg = r!VTPR.vtpr_status_reg,
            ftpr_updated_by = r!VTPR.vtpr_updated_by,
            ftpr_data_5 = r!VTPR.vtpr_data_5,
            ftpr_data_6 = r!VTPR.vtpr_data_6,
            ftpr_data_7 = r!VTPR.vtpr_data_7,
            ftpr_data_8 = r!VTPR.vtpr_data_8,
            ftpr_data_9 = r!VTPR.vtpr_data_9,
            ftpr_data_10 = r!VTPR.vtpr_data_10,
            ftpr_data_11 = r!VTPR.vtpr_data_11,
            ftpr_data_12 = r!VTPR.vtpr_data_12,
            ftpr_data_1 = r!VTPR.vtpr_data_1,
            ftpr_data_2 = r!VTPR.vtpr_data_2,
            ftpr_data_3 = r!VTPR.vtpr_data_3,
            ftpr_data_4 = r!VTPR.vtpr_data_4
        }


data Set = Set Int (DI.IntMap Int)
instance FromJSON Set where
    parseJSON (Object v) = Set <$>
                           v .: "hot_id" <*>
                           v .: "tpr"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero


set :: Connection -> Database -> Set -> IO Int
set rawconn db (Set hot_id tpr) = do
    rowset <- fmap (foldl tree DI.empty) $ getInfo_ db hot_id (jd $ fst $ DI.findMin tpr) (jd $ fst $ DI.findMax tpr)
    withRTSSignalsBlocked $ fmap sum . sequence . fmap fupdate . concatMap DS.toList . DI.elems $ DI.foldWithKey apply rowset tpr
      where tree :: DI.IntMap (DS.Set TprFull) -> TprFull -> DI.IntMap (DS.Set TprFull)
            tree l r = DI.insertWith DS.union (ftpr_year r) (DS.singleton r{ftpr_updated_by = -1}) l
            apply :: Int -> Int -> DI.IntMap (DS.Set TprFull) -> DI.IntMap (DS.Set TprFull)
            apply day tpr_id = DI.update (Just . DS.map applySingle) $ toYear $ jd day
              where applySingle :: TprFull -> TprFull
                    applySingle r =
                      case m of
                        5 -> r{ftpr_data_5 = f $ ftpr_data_5 r}
                        6 -> r{ftpr_data_6 = f $ ftpr_data_6 r}
                        7 -> r{ftpr_data_7 = f $ ftpr_data_7 r}
                        8 -> r{ftpr_data_8 = f $ ftpr_data_8 r}
                        9 -> r{ftpr_data_9 = f $ ftpr_data_9 r}
                        10 -> r{ftpr_data_10 = f $ ftpr_data_10 r}
                        11 -> r{ftpr_data_11 = f $ ftpr_data_11 r}
                        12 -> r{ftpr_data_12 = f $ ftpr_data_12 r}
                        1 -> r{ftpr_data_1 = f $ ftpr_data_1 r}
                        2 -> r{ftpr_data_2 = f $ ftpr_data_2 r}
                        3 -> r{ftpr_data_3 = f $ ftpr_data_3 r}
                        4 -> r{ftpr_data_4 = f $ ftpr_data_4 r}
                      where f = if tpr_id == (fromJust $ ftpr_id r)
                                    then (\x -> x .|. 2 ^ (31 - d))
                                    else (\x -> x .&. (complement $ 2 ^ (31 - d)))
                            m = (\(_, m, _) -> m) $ toGregorian $ jd day
                            d = (\(_, _, d) -> d) $ toGregorian $ jd day
            fupdate :: TprFull -> IO Int
            fupdate r = do
                -- putStrLn $ show $ ftpr_data_5 r
                -- putStrLn $ show $ exprs rcrd
                MMSeason.updateM db rawconn (fromJust $ ftpr_id r) m
              where m = [ftpr_data_5 r, ftpr_data_6 r, ftpr_data_7 r, ftpr_data_8 r,
                         ftpr_data_9 r, ftpr_data_10 r, ftpr_data_11 r, ftpr_data_12 r,
                         ftpr_data_1 r, ftpr_data_2 r, ftpr_data_3 r, ftpr_data_4 r]
                    -- rcrd :: (ShowLabels s, ToPrimExprs s) => (Rel TPR.TPR -> Record s)
                    {- rcrd = (\t -> TPR.tpr_data_5 <<- ti (ftpr_data_5 r)
                                # TPR.tpr_data_6 <<- ti (ftpr_data_6 r)
                                # TPR.tpr_data_7 <<- ti (ftpr_data_7 r)
                                # TPR.tpr_data_8 <<- ti (ftpr_data_8 r)
                                # TPR.tpr_data_9 <<- ti (ftpr_data_9 r)
                                # TPR.tpr_data_10 <<- ti (ftpr_data_10 r)
                                # TPR.tpr_data_11 <<- ti (ftpr_data_11 r)
                                # TPR.tpr_data_12 <<- ti (ftpr_data_12 r)
                                # TPR.tpr_data_1 <<- ti (ftpr_data_1 r)
                                # TPR.tpr_data_2 <<- ti (ftpr_data_2 r)
                                # TPR.tpr_data_3 <<- ti (ftpr_data_3 r)
                                # TPR.tpr_data_4 <<- ti (ftpr_data_4 r)
                                # TPR.tpr_updated_by <<- 1) -}


data Update = Update TprPartial Int
instance FromJSON Update where
    parseJSON (Object v) = Update <$>
                           v .: "row" <*>
                           v .: "tpr_id"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

update :: Connection -> Database -> Update -> IO Int
update rawconn db (Update tpr tpr_id) =
    withRTSSignalsBlocked $ MMSeason.update db rawconn tpr_id rcrd
    where
        rcrd = (\t -> TPR.tpr_name << ((func "RAND") :: Expr String)
                    -- # (maybe (TPR.tpr_name << t!TPR.tpr_name) ((<<-) TPR.tpr_name) (ptpr_name tpr))
                    # TPR.tpr_year << (maybe (t!TPR.tpr_year) constant (ptpr_year tpr))
                    # TPR.tpr_description << (maybe (t!TPR.tpr_description) constant (ptpr_description tpr))
                    # TPR.tpr_hot_id << (maybe (t!TPR.tpr_hot_id) constant (ptpr_hot_id tpr))
                    # TPR.tpr_restriction << (maybe (t!TPR.tpr_restriction) constant (ptpr_restriction tpr))
                    # TPR.tpr_min_nights << (maybe (t!TPR.tpr_min_nights) constant (ptpr_min_nights tpr))
                    # TPR.tpr_status_reg << (maybe (t!TPR.tpr_status_reg) constant (ptpr_status_reg tpr))
                    # TPR.tpr_updated_by <<- 1)


data Insert = Insert Tpr
instance FromJSON Insert where
    parseJSON (Object v) = Insert <$> v .: "row"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

insert :: Connection -> Database -> Insert -> IO Int
insert rawconn db (Insert tpr) = withRTSSignalsBlocked $ MMSeason.insert db rawconn rcrd
  where rcrd = TPR.tpr_id << constNull
             # TPR.tpr_name <<- tpr_name tpr
             # TPR.tpr_year <<- tpr_year tpr
             # TPR.tpr_description <<- tpr_description tpr
             # TPR.tpr_hot_id <<- tpr_hot_id tpr
             # TPR.tpr_restriction <<- tpr_restriction tpr
             # TPR.tpr_min_nights <<- tpr_min_nights tpr
             # TPR.tpr_data_5 <<- "\0\0\0\0"
             # TPR.tpr_data_6 <<- "\0\0\0\0"
             # TPR.tpr_data_7 <<- "\0\0\0\0"
             # TPR.tpr_data_8 <<- "\0\0\0\0"
             # TPR.tpr_data_9 <<- "\0\0\0\0"
             # TPR.tpr_data_10 <<- "\0\0\0\0"
             # TPR.tpr_data_11 <<- "\0\0\0\0"
             # TPR.tpr_data_12 <<- "\0\0\0\0"
             # TPR.tpr_data_1 <<- "\0\0\0\0"
             # TPR.tpr_data_2 <<- "\0\0\0\0"
             # TPR.tpr_data_3 <<- "\0\0\0\0"
             # TPR.tpr_data_4 <<- "\0\0\0\0"
             # TPR.tpr_status_reg <<- "active"
             # TPR.tpr_updated_by <<- 1
             # TPR.tpr_updated << constNull
             # TPR.tpr_created << constNull