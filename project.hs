import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Char

main= do
  putStrLn "Welcome to the Cal function"
  putStrLn "Input year"
  x <- getLine
  putStrLn "Input Month"
  y <- getLine
  let
   -- Convert input into interger
   year=(read x:: Integer)
   -- Convert input into int
   intMonth= (read y :: Int)
   day = days year intMonth

   copy :: Int -> a -> [a]
   copy 0 y = [ ]
   copy x y = y : copy (x-1) y

   map :: (a -> b) -> [a] -> [b]
   map _ [] = []
   map f (x:xs) = f x : map f xs

   add :: [[a]] -> [a]
   add [] = []
   add (x:xs) = x ++ add xs
   -- gregorianMonthLength :: Integer -> Int -> Int
   -- The number of days in i given month according to the proleptic Gregorian calendar. First argument is year, second is month.

   -- showWeekDate :: Day -> String
   -- show in ISO 8601 Week Date format as yyyy-Www-l (m.o. "2006-W46-3").

   -- fromGregorian :: Integer -> Int -> Int -> Day
   -- Convert from proleptic Gregorian calendar. First argument is year, second month number (1-12), third day (1-31). Invalid values will be clipped to the correct range, month first, then day.
   days year intMonth=(x,y)
            where x = gregorianMonthLength year intMonth
                  y = digitToInt(last(showWeekDate (fromGregorian year intMonth 0)))

   cells = (copy x "") ++ [show l | l <- [1..fst(day)] ]
            where x=(snd(day))


   printDay []=[]
   printDay [i] = [[i]]
   printDay [i,j] = [[i,j]]
   printDay [i,j,k] = [[i,j,k]]
   printDay [i,j,k,l] = [[i,j,k,l]]
   printDay [i,j,k,l,m] = [[i,j,k,l,m]]
   printDay [i,j,k,l,m,n] = [[i,j,k,l,m,n]]
   printDay (i:j:k:l:m:n:o:rest) = [[i,j,k,l,m,n,o]] ++ printDay rest

   space s = s ++ (copy (4-(length s)) ' ')
   temp cells =  (add (map space cells)) ++ "\n"
   result cells = add (map temp format)
    where format = printDay cells
  putStrLn  "S | M | T | W | T | F | S "
  putStrLn  (result cells)