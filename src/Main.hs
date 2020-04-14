module Main where

import MerkleTree
import Asset
import Data.ByteString.Char8 (pack)

mockAssetList :: [Asset]
mockAssetList = [b3, b1, b2, b1, b1, b3, b1, b2, b3] 
  where
    pill = Asset (pack "pill_aspirine") []
    b1 = Asset (pack "box_paracetamol")  []
    b2 = Asset (pack "box_morphine")  []
    b3 = Asset (pack "box_aspirine")  [pill, pill, pill]


main :: IO ()
main = putStrLn "Hello, Haskell!"
