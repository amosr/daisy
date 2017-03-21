-- | Attempt at a 'lexer' for LaTeX error logs.
-- LaTeX error logs are abysmal and I hate them very much.
-- This module tries to read them into a stream of tokens.
-- It should require minimal backtracking but that isn't a priority right now.
--
-- The main problem I have with error messages is they use a stack of filenames, and line numbers are displayed
-- for the head of the stack with no filename information.
-- This means you the actual file might be very far away in the log.
-- For example:
--
-- > (./File1                       -- Open File1
-- > (./File2                       -- Open File2
-- > ...
-- > 100 lines of useless logs
-- > ...
-- > )                              -- Close File2
-- > (./File3)                      -- Open File3; Close File3
-- >
-- > l.5 Something interesting!     -- Finally, the error
--
-- The error is actually in ./File1:5, but this isn't obvious from the log.
-- This is complicated by the fact that parentheses can appear elsewhere in warnings or messages:
--
-- > Underfull \hbox (badness 1000) has occurred ... (./File3
--
-- To deal with this, we ignore open parentheses that don't start with filenames including slashes, and ignore their corresponding close parentheses too.
-- What if the warnings/messages themselves contain unbalanced parentheses?
-- At the moment I am assuming that any unclosed ignored parentheses should be forgotten at the end of the line.
--
-- These heuristics are not perfect, but they seem to work most of the time.

module Daisy.Filter.Flat where

import qualified Data.Char as Char
import qualified Data.List as List

-- | Token type
data Flat
 -- | Raw information
 = Message String
 -- | Open file "(FILENAME"
 | OpenFile String
 -- | Close file ")"
 | CloseFile
 -- | Line number in current top of stack "l.NUM"
 | LineNumber Int
 -- | I don't know what these are about but they seem boring. "[NUM]"
 | NoteNumber Int
 -- | Something useful: a line prefixed with a bang "! "
 | Important String
 deriving Show

-- | LaTeX splits lines at 80 chars, including newline.
-- I don't know why, it seems like a stupid thing to do.
-- Were there ever terminal emulators that didn't wrap at the end?
--
-- But we want to unsplit it, because if a filename is really long we'd have
--
-- > (path/to/fil
-- > e/name
--
-- Which should be joined back together as
--
-- > (path/to/file/name
--
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

-- | Parse a single line into a bunch of tokens.
-- The lines should already be joined (above).
parseLine :: String -> [Flat]
parseLine xs
 -- Read line numbers
 | Just xs' <- takePrefix "l." xs
 , Just (num,rest) <- readInt xs'
 = [ LineNumber num, Message $ dropSpace rest ]

 -- Important lines
 | Just xs' <- takePrefix "! " xs
 = [ Important xs' ]

 -- Other junk
 | otherwise
 = parseFileControl xs

-- | Parse the remainder of a line
parseFileControl :: String -> [Flat]
parseFileControl xs0
 | xs0 == ""
 = []

 -- Ignore this, I don't know, whatever
 | Just  xs1        <- takePrefix "[]" $ dropSpace xs0
 = parseFileControl (dropSpace xs1)

 -- Maybe we should ignore these too
 | Just  xs1        <- takePrefix "[" $ dropSpace xs0
 , Just (num,xs2)   <- readInt xs1
 , Just xs3         <- takePrefix "]" xs2
 = go' (NoteNumber num) xs3

 -- Maybe we should ignore these too
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
 = goParens xs0 0 ""

 where
  go' l rest = l : parseFileControl rest

  count c = length . filter (==c)

  -- Try to find any parens with filenames, but be careful to ignore non-filenames.
  -- If there are opens with non-files, we can't treat the closes as CloseFiles.
  goParens [] i acc = msg acc

  goParens ('(':xs1) i acc
   | i == 0
   , Just (file,xs2)  <- readFilename   xs1
   = msg acc ++ [OpenFile file] ++ goParens xs2 0 ""
   | otherwise
   = goParens xs1 (i+1) ('(' : acc)

  goParens (')':xs1) i acc
   | i == 0
   = msg acc ++ [CloseFile] ++ goParens xs1 0 ""
   | otherwise
   = goParens xs1 (i-1) (')' : acc)

  goParens (x:xs1) i acc
   = goParens xs1 i (x : acc)

  msg [] = []
  msg acc = [ Message $ reverse acc ]


readInt :: String -> Maybe (Int,String)
readInt xs
 | ((i,xs'):_) <- reads xs
 = Just (i,xs')
 | otherwise
 = Nothing

readFilename :: String -> Maybe (String,String)
readFilename xs
 = let (fn,xs') = span fnchar xs
   in  if null fn || not (elem '/' fn)
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


