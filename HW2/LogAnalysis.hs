{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log


parse :: String -> [LogMessage]
parse = map parseMessage . lines

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =  map getString . inOrder . build . filter important

getString :: LogMessage -> String
getString (LogMessage _ _ str) = str
getString _ = ""

-- I want these functions to be locally scoped to whatWentWrong because they are not
-- generally useful, but I couldn't find a clean way to do it.

important :: LogMessage -> Bool
important (LogMessage (Error priority) _ _) = priority > 50
important _ = False

parseMessage :: String -> LogMessage
parseMessage str = let wordList = words str in
    case wordList of
      ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
      ("W":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
      ("E":lvl:ts:msg) -> LogMessage (Error (read lvl))
                             (read ts) (unwords msg)
      _ -> Unknown (unwords wordList)

inOrder :: MessageTree -> [LogMessage]
inOrder (Node l msg r) = inOrder l ++ [msg] ++ inOrder r
inOrder Leaf = []

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ newTs _) (Node l n@(LogMessage _ ts _) r)
    | newTs >= ts = Node l n (insert msg r)
    | otherwise = Node (insert msg l) n r

