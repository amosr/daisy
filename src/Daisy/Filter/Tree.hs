module Daisy.Filter.Tree where

import qualified Data.List as List

import qualified Daisy.Filter.Flat as Flat

data Tree
 = File String [Tree]
 | LineNumber Int
 | Message String
 | Important String
 deriving Show

parse :: [Flat.Flat] -> [Tree]
parse fs
 = let (ts,fs') = parse' fs
   in case fs' of
       [] -> ts
       _  -> ts ++ parse fs'

parse' :: [Flat.Flat] -> ([Tree], [Flat.Flat])
parse' [] = ([], [])
parse' (f:fs)
 = case f of
    Flat.Message s
      -> go' (Message s) fs
    Flat.Important s
      -> go' (Important s) fs
    Flat.OpenFile s
     -> let (ts,fs') = parse' fs
        in  go' (File s ts) fs'
    Flat.CloseFile
     -> ([], fs)
    Flat.LineNumber n
     -> go' (LineNumber n) fs
    Flat.NoteNumber _
     -> parse' fs
 where
  go' t fs
   = let (ts,fs') = parse' fs
     in  (t : ts, fs')


skim :: [Tree] -> [Tree]
skim = concatMap go'
 where
  go' t
    = case t of
      File s ts
        -> let ts' = skim ts
           in  if null ts'
               then []
               else [File s ts']
      f -> [f]

localsOnly :: [Tree] -> [Tree]
localsOnly = concatMap go'
 where
  go' t
    = case t of
      File s ts
       | List.isPrefixOf "./" s || any interesting ts
        -> [File s $ localsOnly ts]
       | otherwise
        -> []
      f -> [f]

interestingOnly :: [Tree] -> [Tree]
interestingOnly = go
 where
  go [] = []
  go (t:ts)
   | interesting' t
   = t : ts
   | otherwise
   = go' t ++ go ts

  go' t
   = case t of
      File s ts
       -> let ts' = go ts
          in  if null ts'
              then []
              else [File s ts']
      _ | interesting t
        -> [t]
        | otherwise
        -> []


interesting :: Tree -> Bool
interesting t
 = case t of
    File _ ts -> any interesting ts
    _ -> interesting' t

interesting' :: Tree -> Bool
interesting' t
 = case t of
    File _ ts -> False
    LineNumber{} -> True
    Important{} -> True
    Message{} -> False

