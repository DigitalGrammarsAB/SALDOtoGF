import Saldoer

import Data.List
import System.Environment (getArgs)
import Control.Monad (zipWithM)

main = do
  args <- getArgs
  case args of
    [path] -> do
      let name = "Tot"
      saldom <- readFile path
      let parts = splits $ lines saldom
      putStrLn "read saldo. Writing partitions"
      fpaths <- writeFiles parts 0
      putStrLn "written all files. Extracting ..."
      let skip = 0
      initGFFiles name
      zipWithM (extract skipList name) fpaths [skip..]
      putStrLn "extraction complete.. Completing files ..."
      endGFFiles name
    _ -> do
      putStrLn "Please specify file to extract"

partName :: Int -> String
partName n = "data/saldoPart"++show n++".json"

skipList :: Maybe [String]
skipList = Nothing

-- | Split into chunks, but make sure a lexgram id isn't split between chunks
splits :: [String] -> [[String]]
splits []  = []
splits xs = (part++end):splits rest'
  where
    (part,rest) = splitAt 200000 xs
    lastid = findID $ last part
    (end,rest') = span (\s -> findID s == lastid) rest

    findID :: String -> String
    findID s = case findSubstring "\"id\"" s of
      Just n -> takeWhile ((/=)'"') $ drop (n+6) s
      Nothing -> ""

-- | Like elemIndex but for substrings
findSubstring :: Eq a => [a] -> [a] -> Maybe Int
findSubstring a b = find 0 a b
  where
    find x a b
      | null a || null b = Nothing
      | a `isPrefixOf` b = Just x
      | otherwise = find (x+1) a (tail b)

writeFiles :: [[String]] -> Int -> IO [FilePath]
writeFiles [] _ = return []
writeFiles (x:xmls) n = do
     putStrLn $ "Writing part "++show n
     let name = partName n
     writeFile name (unlines x)
     names <- writeFiles xmls (n+1)
     return $ name:names

initGFFiles :: String -> IO ()
initGFFiles tot = do
  writeFile ("saldo"++tot++".gf")   $ absHeader "Tot" ""
  writeFile ("saldo"++tot++"Cnc.gf") $ concHeader "Tot" ""

endGFFiles :: String -> IO ()
endGFFiles tot = do
  appendFile ("saldo"++tot++".gf") "}"
  appendFile ("saldo"++tot++"Cnc.gf") "}"
