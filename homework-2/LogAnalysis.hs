{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import           Log
import           Text.Read                      ( readMaybe )

parseError :: [String] -> Maybe (TimeStamp -> String -> LogMessage, [String])
parseError []       = Nothing
parseError (x : xs) = case m of
  Just n  -> Just (LogMessage (Error n), xs)
  Nothing -> Nothing
  where m = readMaybe x :: Maybe Int

parseMessageType
  :: [String] -> Maybe (TimeStamp -> String -> LogMessage, [String])
parseMessageType ("I"     : xs) = Just (LogMessage Info, xs)
parseMessageType ("W"     : xs) = Just (LogMessage Warning, xs)
parseMessageType ("E" : n : xs) = case readMaybe n of
  Just x  -> Just (LogMessage (Error x), xs)
  Nothing -> Nothing
parseMessageType _ = Nothing

parseTimestamp
  :: Maybe (TimeStamp -> String -> LogMessage, [String])
  -> Maybe (String -> LogMessage, [String])
parseTimestamp Nothing            = Nothing
parseTimestamp (Just (_, []    )) = Nothing
parseTimestamp (Just (f, x : xs)) = case m of
  Just n  -> Just (f n, xs)
  Nothing -> Nothing
  where m = readMaybe x

parseString :: Maybe (String -> LogMessage, [String]) -> Maybe LogMessage
parseString Nothing        = Nothing
parseString (Just (f, xs)) = Just (f (unwords xs))

parseMessageInner :: String -> Maybe LogMessage
parseMessageInner = parseString . parseTimestamp . parseMessageType . words

parseMessage :: String -> LogMessage
parseMessage s = case result of
  Just x  -> x
  Nothing -> Unknown s
  where result = parseMessageInner s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert logMessage  Leaf        = Node Leaf logMessage Leaf
insert logMessage messageTree
  | newTimestamp < currentTimestamp = Node (insert logMessage left) entry right
  | otherwise                       = Node left entry (insert logMessage right)
 where
  (LogMessage _    newTimestamp                            _    ) = logMessage
  (Node       left entry@(LogMessage _ currentTimestamp _) right) = messageTree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMessage right) =
  inOrder left ++ [logMessage] ++ inOrder right

getString :: LogMessage -> String
getString (Unknown string       ) = string
getString (LogMessage _ _ string) = string

isHighSeverity :: LogMessage -> Bool
isHighSeverity (LogMessage (Error n) _ _) = n > 50
isHighSeverity _                          = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getString . filter isHighSeverity . inOrder . build
