{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage str =
    let wordList = words str
     in case wordList of
            ("E" : sv : ts : ms) -> LogMessage (Error (read sv)) (read ts) (unwords ms)
            ("I" : ts : ms) -> LogMessage Info (read ts) (unwords ms)
            ("W" : ts : ms) -> LogMessage Warning (read ts) (unwords ms)
            _ -> Unknown str

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert lms@LogMessage{} Leaf = Node Leaf lms Leaf
insert lms@(LogMessage _ ts1 _) (Node left nms@(LogMessage _ ts2 _) right)
    | ts1 > ts2 = Node left nms (insert lms right)
    | otherwise = Node (insert lms left) nms right
insert _ node = node

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lms right) = inOrder left ++ [lms] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extract . inOrder . build . filter (severeLargeThan 50)
  where severeLargeThan severity (LogMessage (Error sv) _ _) = sv >= severity
        severeLargeThan _ _ = False
        extract (LogMessage _ _ msg) = msg
        extract _ = ""
