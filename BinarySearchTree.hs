module BinarySearchTree where

import Prelude hiding (lookup)
import Data.List (intercalate)

data BST = Leaf
         | InternalNode Int String BST BST

-- Initialize tree for testing
myTree :: BST
myTree = InternalNode 5 "Five"
         (InternalNode 3 "Three"
             (InternalNode 1 "One" Leaf Leaf) Leaf)
         (InternalNode 8 "Eight" Leaf Leaf) 

{-
Tree Structure = 
     5
    / \
   3   8
  /
 1
 -}

search :: Int -> BST -> Maybe String -- Maybe is used in case nothing is found
search soughtKey (InternalNode currentKey currentItem leftChild rightChild)
   | currentKey == soughtKey = Just currentItem
   | soughtKey < currentKey = search soughtKey (leftChild) 
   | otherwise = search soughtKey (rightChild)
search _ Leaf = Nothing

insert :: Int -> String -> BST -> BST
insert userKey userItem Leaf = InternalNode userKey userItem Leaf Leaf -- If on leaf, insert Key and Item here
insert userKey userItem (InternalNode currentKey currentItem leftChild rightChild) 
    | userKey == currentKey = InternalNode currentKey userItem leftChild rightChild
    | userKey < currentKey = InternalNode currentKey currentItem (insert userKey userItem leftChild) rightChild
    | userKey > currentKey  = InternalNode currentKey currentItem leftChild (insert userKey userItem rightChild)

displayTree :: BST -> String
displayTree Leaf = ""
displayTree (InternalNode key item left right) =
    intercalate " " $ filter (not . null) [displayTree left, show key ++ ":" ++ item, displayTree right] -- 'filter' used for formatting string
    
delete :: Int -> BST -> BST
delete _ Leaf = Leaf
delete userKey (InternalNode currentKey currentItem leftChild rightChild)
    | userKey < currentKey = InternalNode currentKey currentItem (delete userKey leftChild) rightChild
    | userKey > currentKey = InternalNode currentKey currentItem leftChild (delete userKey rightChild)
    | otherwise = 
        case (leftChild, rightChild) of 
            (Leaf, Leaf) -> Leaf
            (Leaf, _) -> rightChild
            (_, Leaf) -> leftChild
            (_, _) -> let (replacementKey, replacementItem) = findMin rightChild
                      in InternalNode replacementKey replacementItem leftChild (delete replacementKey rightChild)

-- Helper function for handling worse case deletion scenario
findMin :: BST -> (Int, String)
findMin (InternalNode key item leaf _) = (key, item) 
findMin (InternalNode _ _ leftChild _) = findMin leftChild
findMin Leaf = error "Empty tree!"