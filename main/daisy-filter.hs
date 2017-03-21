import qualified Daisy.Filter as Filter

main = getContents >>= mapM putStrLn . Filter.out

