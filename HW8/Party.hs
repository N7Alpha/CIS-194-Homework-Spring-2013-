{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Tree
import Data.Monoid
import Data.List

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL list fun) = GL (emp:list) (fun + empFun emp)

instance Monoid GuestList where
    mappend (GL list1 fun1) (GL list2 fun2) = GL (list1 ++ list2) (fun1 + fun2)
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f z (Node rl sf) = f rl (map (treeFold f z) sf)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss withWithout = (mconcat $ map (uncurry max) withWithout, foldr (<>) bossList without)
        where without = map snd withWithout
              bossList = GL [boss] (empFun boss)
      
maxFun :: Tree Employee -> GuestList
maxFun tree = uncurry max res
        where res = treeFold nextLevel (mempty, mempty) tree

main :: IO ()
main = readFile "company.txt" >>= (putStr . prettyPrint . maxFun . read)
    where prettyPrint (GL list fun) = unlines $ ("Total fun: " ++ show fun) : sort (map empName list)