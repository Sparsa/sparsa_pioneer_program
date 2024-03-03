isJust :: Maybe a -> Bool
isJust dat = case dat of 
 Nothing -> False
 Just _ -> True

isNothing :: Maybe a -> Bool
isNothing dat = not (isJust dat)

mayybee :: b -> ( a -> b ) -> Maybe a -> b
mayybee b f ob = case ob of 
 Nothing -> b
 Just a -> f a
-- used the identiy function here using lambda
fromMaybe :: a -> Maybe a -> a
fromMaybe acc ob = mayybee acc (\x -> x) ob

listToMaybe :: [a] -> Maybe a
listToMaybe  list = case list of 
 [] -> Nothing
 (x:_) -> Just x

mayBeToList :: Maybe a -> [a]
mayBeToList obj = case obj of
 Nothing -> []
 Just x -> [x]

catMaybes :: [Maybe a] -> [a]
catMaybes may_list = case may_list of 
 [] -> []
 (x:xs) -> (mayBeToList x) ++ (catMaybes  xs)
