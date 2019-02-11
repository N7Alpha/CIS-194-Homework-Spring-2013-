{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Main where

import Buffer
import Editor
import Sized
import JoinList
import Scrabble
import Data.Monoid

-- Exercise 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag (Append mon _ _) = mon
tag (Single mon _) = mon
tag Empty = mempty

-- Exercise 2

-- Helper function
treeSize :: (Sized b, Monoid b) => JoinList b a -> Int
treeSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) =>
    Int -> JoinList b a -> Maybe a
indexJ i root@(Append _ left right)
    | i >= treeSize root = Nothing
    | i >= treeSize left = indexJ (i - treeSize left) right
    | otherwise = indexJ i left
indexJ _ (Single _ s) = Just s
indexJ _ Empty = Nothing

dropJ :: (Sized b, Monoid b) =>
    Int -> JoinList b a -> JoinList b a
dropJ num root@(Append mon left right) 
    | num >= treeSize root = Empty
    | num > treeSize left = Empty +++ dropJ (num - treeSize left) right
    | otherwise = dropJ num left +++ right
dropJ 1 _ = Empty
dropJ 0 a = a

takeJ :: (Sized b, Monoid b) =>
    Int -> JoinList b a -> JoinList b a
takeJ num root@(Append size left right)
    | num > treeSize root = root
    | num > treeSize left = left +++ takeJ (num - treeSize left) right
    | otherwise = takeJ num left +++ Empty
takeJ 1 s = s
takeJ 0 _ = Empty

-- Exercise 4
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

instance Buffer (JoinList (Score, Size) String) where

    toString = unlines . jlToList

    fromString = fromLines . lines where
        fromLines [] = Empty
        fromLines [l] = Single (scoreString l, Size 1) l
        fromLines ls = fromLines (take half ls) +++ fromLines (drop half ls)
             where half = length ls `div` 2

    line = indexJ
  
    replaceLine _ _ Empty = Empty 
    replaceLine n _ buf | n < 0 || n >= treeSize buf = buf
    replaceLine n ln buf = takeJ n buf +++ Single (scoreString ln, Size 1) ln +++ dropJ (n + 1) buf
  
    numLines = treeSize
    value = getScore . fst . tag

initBuf :: JoinList (Score, Size) String 
initBuf = fromString $ unlines
             [ "This buffer is for poems you want to discard,"
             , "and for tinkering with steam valve coefficients."
             , "To load a different file, type the character 'l'"
             , "followed by the name of the file."
             ]
                
main :: IO ()
main = runEditor editor initBuf