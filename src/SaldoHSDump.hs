
-- | Read/save lexicon from/to dumped Haskell term
-- Mainly for speeding up testing

module SaldoHSDump (parseDict, dumpDict) where

import Common hiding (show)

parseDict :: FilePath -> IO (Maybe Lex)
parseDict path = do
  s <- readFile path
  return $ Just $ read s

-- | Dump dictionary to file
dumpDict :: FilePath -> Lex -> IO ()
dumpDict path lexi =
  writeReportFile path (show lexi)
