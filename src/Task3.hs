{-# OPTIONS_GHC -Wall #-}
module Task3 where

import Prelude hiding (compare, foldl, foldr, Ordering(..))
import Task1 (Tree(..))
import Task2 (tlookup, tinsert, tdelete, bstToList, listToBST, compare, Ordering)

-- | Tree-based map
type Map k v = Tree (k, v)

-- | Helper comparison function that compares only the keys.
compareKeys :: Ord k => (k, v) -> (k, v) -> Ordering
compareKeys (k1, _) (k2, _) = compare k1 k2

-- | Constructs a map from an association list.
listToMap :: Ord k => [(k, v)] -> Map k v
listToMap = listToBST compareKeys

-- | Converts a map into a sorted association list.
mapToList :: Map k v -> [(k, v)]
mapToList = bstToList

-- | Looks up a value by key in the map.
mlookup :: Ord k => k -> Map k v -> Maybe v
mlookup key m = case tlookup compareKeys (key, undefined) m of
                  Just (_, v) -> Just v
                  Nothing     -> Nothing

-- | Inserts a key and value into the map.
minsert :: Ord k => k -> v -> Map k v -> Map k v
minsert key val = tinsert compareKeys (key, val)

-- | Deletes a key (and its associated value) from the map.
mdelete :: Ord k => k -> Map k v -> Map k v
mdelete key = tdelete compareKeys (key, undefined)