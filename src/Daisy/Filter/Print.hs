module Daisy.Filter.Print where

import qualified Daisy.Filter.Tree as Tree

out :: [Tree.Tree] -> [String]
out = concatMap (out' [])


out' :: [String] -> Tree.Tree -> [String]
out' fs t
 = case t of
    Tree.File s ts
     -> line (coloured 0 33 s)
      ++ concatMap (out' (s : fs)) ts

    Tree.LineNumber i
     | (f:_) <- fs
     -> [ coloured 4 31 (f ++ ":" ++ show i) ]
     | otherwise
     -> [ coloured 4 31 ("main:" ++ show i) ]

    Tree.Message s
     -> line s
    Tree.Important s
     -> line $ coloured 0 31 s

 where
  line xs
   = [ replicate (length fs) ' ' ++ xs ]


coloured :: Int -> Int -> String -> String
coloured a b ss
 = "\ESC[" ++ show a ++ ";" ++ show b ++ "m" ++ ss ++ "\ESC[0m"
