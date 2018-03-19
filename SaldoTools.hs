module SaldoTools where
import Data.List
import Data.Char
import System.FilePath.Posix
import System.Directory

import Saldoer (absHeader, concHeader)

skipList :: Maybe [String]
skipList = Nothing

-- | Split into chunks, but make sure a lexgram id isn't split between chunks
splits []  = []
splits xs = let (part,rest) = splitAt 200000 xs
                lastid = findID $ last part
                (end,rest') = span (\s -> findID s == lastid) rest
            in (part++end):splits rest'

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
     let name = "data/saldoPart"++show n++".json"
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

dropWhites :: String -> String
dropWhites = takeWhile (not . isSpace) . dropWhile isSpace

noOfParts :: IO [Int]
noOfParts = do
  fs <- getDirectoryContents "."
  return $ sort [ read $ dropWhile (not . isNumber) p | p <- map dropExtensions fs
                   ,"saldoPart" `isPrefixOf` p ]
