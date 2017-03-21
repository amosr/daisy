module Daisy.Filter.Flat where

import qualified Data.Char as Char
import qualified Data.List as List


data Flat
 = Message String
 | OpenFile String
 | CloseFile
 | LineNumber Int
 | NoteNumber Int
 | Important String
 deriving Show

joinLines :: [String] -> [String]
joinLines [] = []
joinLines (x:xs)
 | length x == 79
 , not (List.isSuffixOf "..." x)
 = case joinLines xs of
    []      -> x : []
    (y:ys)  -> (x ++ y) : ys

 | otherwise
 = x : joinLines xs

parseLine :: String -> [Flat]
parseLine xs
 | Just xs' <- takePrefix "l." xs
 , Just (num,rest) <- readInt xs'
 = [ LineNumber num, Message $ dropSpace rest ]

 | Just xs' <- takePrefix "! " xs
 = [ Important xs' ]

 | otherwise
 = parseFileControl xs


parseFileControl :: String -> [Flat]
parseFileControl xs0
 | xs0 == ""
 = []

 | Just  xs1        <- takePrefix "[]" $ dropSpace xs0
 = parseFileControl (dropSpace xs1)

 | Just  xs1        <- takePrefix "[" $ dropSpace xs0
 , Just (num,xs2)   <- readInt xs1
 , Just xs3         <- takePrefix "]" xs2
 = go' (NoteNumber num) xs3

 | Just xs1         <- takePrefix "(" $ dropSpace xs0
 , Just (file,xs2)  <- readFilename   xs1
 = go' (OpenFile file) xs2

 | Just xs1 <- takePrefix ")" $ dropSpace xs0
 = go' CloseFile xs1

 | Just xs1         <- takePrefix "{" $ dropSpace xs0
 , Just (file,xs2)  <- readFilename   xs1
 , Just xs3         <- takePrefix "}" xs2
 = parseFileControl xs3

 | Just xs1         <- takePrefix "<" $ dropSpace xs0
 , Just (file,xs2)  <- readFilename   xs1
 , Just xs3         <- takePrefix ">" xs2
 = parseFileControl xs3

 | otherwise
 = let opens = count '(' xs0
       closes = count ')' xs0
       (xs1,xs2) = break (\c -> c == '(' || c == ')') xs0
   in  if closes /= opens
       then Message xs1 : parseFileControl xs2
       else [ Message xs0 ]
 where
  go' l rest = l : parseFileControl rest

  count c = length . filter (==c)


readInt :: String -> Maybe (Int,String)
readInt xs
 | ((i,xs'):_) <- reads xs
 = Just (i,xs')
 | otherwise
 = Nothing

readFilename :: String -> Maybe (String,String)
readFilename xs
 = let (fn,xs') = span fnchar xs
   in  if null fn
       then Nothing
       else Just (fn,xs')
 where
  fnchar c = not (Char.isSpace c) &&
             not (elem c "()<>{}")

takePrefix :: String -> String -> Maybe String
takePrefix pre xs
 | List.isPrefixOf pre xs
 = Just $ List.drop (length pre) xs
 | otherwise
 = Nothing

dropSpace :: String -> String
dropSpace = List.dropWhile Char.isSpace


