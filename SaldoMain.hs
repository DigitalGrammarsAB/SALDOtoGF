import Saldoer
import SaldoTools

import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import Control.Monad (zipWithM)

main = do
  arg <- fmap (fmap read . listToMaybe) getArgs
  (fpaths,skip) <- case arg of
    Nothing -> do
        saldom <- readFile "data/saldo.json"
        let parts = splits $ lines saldom
        putStrLn "read saldo. Writing partitions"
        fpaths <- writeFiles parts 0
        putStrLn "written all files. Extracting ..."
        return (fpaths,0)
    Just skip -> do
        -- Remember magic constant 9. Use getDirectoryContents to fix
        return (["data/saldoPart"++show n++".json" | n <- [skip..9]],skip)
  initGFFiles "Tot"
  zipWithM (extract skipList "Tot") fpaths [skip..]
  putStrLn "extraction complete.. Completing files ..."
  endGFFiles "Tot"
  putStrLn "Lexicon salodTot created!"
