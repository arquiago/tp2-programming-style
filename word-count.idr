import Data.String
import Data.List
import System.File.ReadWrite
import Data.SortedMap 


partial aHead : String -> Char
aHead "" = '\0'
aHead x  = strIndex x 0

checkNum : Char -> String
checkNum x = if isAlpha x then singleton x else ""

partial removeNum : String -> String
removeNum "" = ""
removeNum x  = let elem = checkNum (aHead x) in 
                    let elem' = removeNum (strTail x) in
                    elem ++ elem'

normalize : String -> String
normalize s = toLower s

verify' : String -> List String -> Bool
verify' s sl = foldr (\elem, acc => (elem /= s) && acc) True sl

stopWords : String -> IO (List String)
stopWords path = do result <- readFile path
                    case result of 
                        Right ls => pure (words ls)
                        Left err => pure ([""])

purify : IO (List String) -> List String
purify ls = unsafePerformIO ls

removeStopWords : List String -> List String -> List String
removeStopWords wordstc stopw = [x | x <- wordstc, verify' x stopw]

countWords : List String -> SortedMap String Int -> SortedMap String Int
countWords [] sm = sm
countWords (s :: ls) sm = case (lookup s sm) of
                              Nothing => countWords ls (insert s 1 sm)
                              Just x => countWords ls (insert s (x + 1) sm)
take25 : List a -> List a 
take25 ls = take 25 ls
top_tfive xs = List.take 25 (sorted xs)
            where
              my_compare: (String,Int) -> (String, Int) -> Ordering
              my_compare (x1, y1) (x2, y2) = compare y2 y1

              sorted : List (String, Int) -> List (String, Int)
              sorted xs = sortBy my_compare xs

partial main : IO ()
main = do
    path <- getLine
    Right drum <- readFile path
        | Left err => putStr "Falha na leitura do arquivo"
    stopPath <- getLine 
    let stop = stopWords stopPath
    putStr(show (((top_tfive (Data.SortedMap.toList(countWords (removeStopWords (words drum) (purify stop)) empty))))))


