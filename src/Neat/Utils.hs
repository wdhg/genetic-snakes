module Neat.Utils where

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ []
  = []
replaceAt 0 item (_ : items')
  = item : items'
replaceAt index item (item' : items)
  = item' : replaceAt (index - 1) item items

getIndex :: Eq a => a -> [a] -> Maybe Int
getIndex _ []
  = Nothing
getIndex item (item':items)
  | item == item' = Just 0
  | otherwise = succ <$> getIndex item items
