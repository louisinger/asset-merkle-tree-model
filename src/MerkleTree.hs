{- |
Module      :  MerkleTree.hs
Description :  Merkle tree implementation for asset merkle tree model.
License     :  unlicense

Maintainer  :  louis.singer.git@gmail.com
Stability   :  experimental
Portability :  portable
-}

module MerkleTree where

  import Data.Text (Text)
  import Crypto.Hash.SHA256 (hash)
  import Data.ByteString.Encoding (encode, decode, utf8)
  import Data.ByteString (ByteString)
  import Data.ByteString.Char8 (pack)

  data Tree = Leaf ByteString | Node Tree ByteString Tree 
    deriving (Show)

  getData :: Tree -> ByteString
  getData (Leaf bs) = bs
  getData (Node _ bs _) = bs

  char2bytes :: String -> ByteString
  char2bytes = pack

  leaves :: [ByteString] -> [Tree]
  leaves = map Leaf

  strings2leaves :: [String] -> [Tree]
  strings2leaves xs = leaves (map char2bytes xs)

  encodeUtf8 :: Text -> ByteString
  encodeUtf8 = encode utf8

  decodeUtf8 :: ByteString -> Text
  decodeUtf8 = decode utf8

  concatenate :: Tree -> Tree -> ByteString
  concatenate l r = getData l <> getData r

  node :: Tree -> Tree -> Tree
  node l r = Node l (hash (concatenate l r)) r

  nextLayer :: [Tree] -> [Tree]
  nextLayer [] = []
  nextLayer [l] = [l]
  nextLayer (l:r:xs) = node l r : nextLayer xs 

  merkleRoot :: [Tree] -> Tree
  merkleRoot [] = error "empty list"
  merkleRoot [l] = l
  merkleRoot ts = merkleRoot (nextLayer ts)

