import Data.Char
notThe :: String -> Maybe String
notThe string 
 | ( Prelude.map Data.Char.toLower string) == "the" = Nothing
 | otherwise = Just string

replaceThe :: String -> String
replaceThe string
 | string == "" = string
 | notThe ( head $ words string) == Nothing = "a " ++ replaceThe ( drop ((length (head $ words string))+1) string)
 | otherwise =  (head $ words string) ++ " " ++ replaceThe ( drop ((length (head $ words string))+1)  string)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel string 
 | string == "" = 0
 | notThe (head $ words string) == Nothing = (if (head  (drop ((length (head $ words string)) +1 ) string) `elem` "aeiou") then 1 else 0 )+ countTheBeforeVowel (drop ((length (head $ words string)) +1 ) string)
 | otherwise = 0 + countTheBeforeVowel (drop ((length (head $ words string)) +1 ) string)

countVowels :: String -> Integer
countVowels string 
 | string == "" = 0
 | (Data.Char.toLower (head string)) `elem` "aeiou" = 1 + (countVowels $ tail string)
 | otherwise = 0 + (countVowels $ tail string)


