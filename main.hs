import System.IO
import System.Directory 
import Text.Read
import Data.List
import Data.Char (toLower)

data SpendCatagory = Groceries | Online | Transport | Restaraunt | Misc
    deriving (Show, Read)
type Money = Double 
type SpendBook = [(SpendCatagory, Money)]

main :: IO ()
main = do
    all <- listDirectory directory
    handle <- openFile ( buildFilePath directory (extractOneFile all) ) ReadMode
    contents <- hGetContents handle
    let array = map splitLines . tail $ lines contents
    let valueChange = findTotalChange array
    let book = map formatSpendBook (categoriseCosts array)
    mapM_ putStrLn book
    if valueChange > 0
        then putStrLn $ "\ESC[92mIncrease in value of this account: " ++ show valueChange
    else
        putStrLn $ "\ESC[91mDecrease in value of this account: " ++ show valueChange
    hClose handle
    where
        directory = "BankFiles"

categoriseCosts:: [[String]] -> SpendBook
categoriseCosts array = 
    [(Groceries, foldl (addIfStringMatch groceries) 0 array ),
     (Online, foldl (addIfStringMatch online) 0 array),
     (Transport, foldl (addIfStringMatch transport) 0 array),
     (Restaraunt, foldl (addIfStringMatch restaraunt) 0 array),
     (Misc, foldl (addIfStringDoesntMatch all) 0 array)]
    where 
        groceries = ["asda", "sainsbury", "tesco", "waitrose", "marks and spensers", "m and s", "m & s", "aldi", "lidl" ]
        online = ["amazon", "ebay", "paypal"]
        transport = ["tfl", "train", "bus", "taxi", "metro", "bike"]
        restaraunt = ["restaraunt", "food" ]
        all = groceries ++ online ++ transport ++ restaraunt
    
formatSpendBook:: (SpendCatagory, Money) -> String
formatSpendBook (cata, x) = 
    ("Change in balance due to " ++ show cata ++ " was: " ++ show x) 

addIfStringDoesntMatch:: [String] -> Double -> [String] -> Double
addIfStringDoesntMatch typ acc x
    | stringInCat word typ = acc 
    | otherwise = acc + read (x!!2)
    where 
        word = (x!!1)

addIfStringMatch:: [String] -> Double -> [String] -> Double
addIfStringMatch typ acc x
    | stringInCat word typ = acc + read (x!!2)
    | otherwise = acc
    where 
        word = (x!!1)

stringInCat:: String -> [String] -> Bool
stringInCat s [] = False
stringInCat s (x:xs)
    | isInfixOf x (map toLower s) = True
    | otherwise = stringInCat s xs

findTotalChange:: [[String]] -> Double
findTotalChange array = (read latest::Double) - (read earliest::Double)
    where latest = last (head array)
          earliest = last (last array)

splitLines:: String -> [String]
splitLines s = case dropWhile (==',') s of
                      "" -> []
                      s' -> w : splitLines s''
                            where (w, s'') = break (==',') s'

buildFilePath:: String -> String -> String
buildFilePath directory filename = directory ++ "/" ++ filename

extractOneFile:: [String] -> String
extractOneFile x 
    |   x == [] = error "You need to add a file to the BankFile Directory"
    |   otherwise = head x

moneyLeft:: (RealFloat a) => a -> String
moneyLeft x
    |   x <= 10 = "Your practically broke, save!"
    |   x <= 100 = "You have enough for food, congratulations"
    |   x <= 1000 = "Cash money"
    |   otherwise = "Stop selling drugs / pay your tuition, you shouldn't be this rich."
