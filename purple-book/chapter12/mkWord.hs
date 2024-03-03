newtype Word' = 
  Word' String
 deriving (Eq,Show)

vowels = "aeiou"
countVowels :: String -> Integer
countVowels string = foldr (\x acc -> if x `elem` vowels then acc+1 else acc ) 0 string

countConsonents :: String -> Integer
countConsonents string = foldr (\x acc -> if x `elem` vowels then acc else acc + 1) 0 string

countAll :: String -> Integer
countAll string = foldr (\x acc -> if x `elem` vowels then acc+1 else acc -1 ) 0 string


mkWord :: String -> Maybe Word'
mkWord string = if (foldr (\x acc -> if x `elem` vowels then acc + 1 else acc -1 ) 0 string) >= 0  then Nothing else Just (Word' (string))

