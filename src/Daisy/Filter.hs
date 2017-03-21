module Daisy.Filter where

import qualified Daisy.Filter.Flat as Flat
import qualified Daisy.Filter.Tree as Tree
import qualified Daisy.Filter.Print as Print

logs :: String -> [Tree.Tree]
logs xs
 = let xs' = lines xs
       joined = Flat.joinLines xs'
       flats = concatMap Flat.parseLine joined
       trees = Tree.parse flats
   in trees

filteredOut :: [Tree.Tree] -> [String]
filteredOut trees
 = let locals = Tree.localsOnly trees
       skim = Tree.skim locals
   in  Print.out skim

out :: String -> [String]
out xs
 = let full   = logs xs
       full'  = filteredOut full
       errs   = Tree.interestingOnly full
       errs'  = filteredOut errs

       errs'' = if null errs'
                then [Print.coloured 0 32 "No errors"]
                else [Print.coloured 0 31 "Errors:"] ++ errs'
   in  full' ++ [""] ++ errs''

