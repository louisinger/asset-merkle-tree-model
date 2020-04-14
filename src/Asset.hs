module Asset where

  import Data.ByteString (ByteString)
  import Crypto.Hash.SHA256 (hash)
  import MerkleTree (Tree, node)

  data Asset = Asset { 
    typeOf :: ByteString,
    assets :: [Asset]
  }

  sortByType :: [Asset] -> [[Asset]]

  assetTree :: Asset -> Tree
  assetTree a = 