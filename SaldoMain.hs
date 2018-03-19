import Saldoer
import SaldoTools

import System.Environment (getArgs)

-- main = do
--   arg <- fmap (fmap read . listToMaybe) getArgs
--   (fpaths,skip) <- case arg of
--     Nothing -> do
--         saldom <- readFile  ""
--         let parts = splits $ lines saldom
--         putStrLn "read saldo. Writing partitions"
--         fpaths <- writeFiles parts 0
--         putStrLn "written all files. Extracting ..."
--         return (fpaths,0)
--     Just skip -> do
--         -- Remember magic constant 15. Use getDirectoryContents to fix
--         return (["saldoPart"++show n++".xml" | n <- [skip..15]],skip)
--   initGFFiles "Tot"
--   zipWithM (extract skipList "Tot") fpaths [skip..]
--   putStrLn "extraction complete.. Completing files ..."
-- --  combine files
--   endGFFiles "Tot"
--   putStrLn "Lexicon salodTot created!"
-- --  combine files
--   return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    path:_ -> do
      let name = "Tot"
      initGFFiles name
      extract Nothing name path 0
      endGFFiles name
    _ -> do
      putStrLn "Please specify path to JSON file"
