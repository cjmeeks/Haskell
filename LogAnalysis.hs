{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
 import Log
 parseMessage :: String -> LogMessage
 parseMessage str = case words str of
                     ("I" : ts : msg) -> LogMessage Info (read ts) (unwords msg)
                     ("W" : ts : msg) -> LogMessage Warning (read ts) (unwords msg)
                     ("E" : num : ts : msg) -> LogMessage (Error (read num)) (read ts) (unwords msg)

 insert :: LogMessage -> MessageTree -> MessageTree

 insert logMsg1@(LogMessage _ time1 _) (Node left logMsg2@(LogMessage _ time2 _) right)
  | time1 > time2 = Node left logMsg2 (insert logMsg1 right)
  | otherwise     = Node (insert logMsg1 left) logMsg2 left
 insert _ tree = tree

 build :: [LogMessage] -> MessageTree
 build (x:lms) = insert x (build lms)

 inOrder :: MessageTree -> [LogMessage]
 inOrder (Node left lgmsg@(LogMessage _ _ _) right) = inOrder left ++ [lgmsg] ++ inOrder right

 whatWentWrong :: [LogMessage] -> [String]
 whatWentWrong ((LogMessage (Error num) _ msg) : lms)
  | num >= 50 = [msg] ++ whatWentWrong lms
  | otherwise = [] ++ whatWentWrong lms
