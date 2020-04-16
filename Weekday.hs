-----------------------------------------------------------------------
-- 
-- Sevan Golnazarian
-- Simple Date App that calculates which day of the week it will be after n days
--
-----------------------------------------------------------------------
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Calendar.WeekDate

data WeekDay = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

main = do
    putStrLn "how many days from now?"
    n <- getLine
    nDate <- weekDay $ nextNDay (read n::Int) localDate
    let wDay = toWeekDay nDate
    putStrLn $ n ++ " days from now is a " ++ (show wDay)

date :: IO (Integer, Int, Int)
date = getCurrentTime >>= return . toGregorian . utctDay

localDate :: IO(Day)
localDate = localDay <$> zoneNow where 
    zoneNow = getCurrentTimeZone >>= \z -> getCurrentTime >>= \t -> return $ utcToLocalTime z t

nextDay :: IO(Day) -> IO(Day)
nextDay day = succ <$> day

nextNDay :: Int -> IO(Day) -> IO(Day)
nextNDay 0 d = d
nextNDay n d = 
    let d' = nextDay d in
        nextNDay (n - 1) d'
        
toWeekDay :: Int -> WeekDay
toWeekDay n = toEnum x::WeekDay where
    x = n `mod` 7

weekDay :: IO(Day) -> IO(Int)
weekDay day = wday where
    wday = toWeekDate <$> day >>= \(_,_,d) -> return d