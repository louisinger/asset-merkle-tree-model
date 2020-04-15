module Asset where

  import Data.List (groupBy, sortOn)
  import Data.ByteString (ByteString)
  import Crypto.Hash.SHA256 (hash)
  import MerkleTree (Tree (Leaf, Node), node, merkleRoot)
  import Data.Ord (comparing)

  data Asset = Asset { 
    typeOf :: ByteString,
    content :: [Asset]
  } deriving (Show)

  sortByType :: [Asset] -> [Asset]
  sortByType = sortOn typeOf

  groupByType :: [Asset] -> [[Asset]]
  groupByType = groupBy (\a a' -> typeOf a == typeOf a')

  sortAndGroup :: [Asset] -> [[Asset]]
  sortAndGroup = groupByType . sortByType

  typeTree :: Asset -> Tree
  typeTree = Leaf . hash . typeOf

  contentTree :: Asset -> Tree
  contentTree a = merkleRoot [(merkleRoot . map assetTree) as | as <- (sortAndGroup . content) a]

  assetTree :: Asset -> Tree
  assetTree (Asset t []) = typeTree (Asset t [])
  assetTree a = merkleRoot [typeTree a, contentTree a]