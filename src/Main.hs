
-- | This module takes care of command-line arguments and splitting the input data files

import Saldoer (doExtract)

import qualified Data.Map.Strict as M
import Data.List (splitAt)
import System.Environment (getArgs)
import System.FilePath (takeExtension)
import Control.Monad (when)
import Text.Printf

import qualified SaldoXML as XML
import qualified SaldoJSON as JSON

-- | How many lexical entries to process at a time with GF
chunkSize :: Int
-- chunkSize = 4 -- testing
chunkSize = 10000 -- (prev: 200000 lines of JSON)

main :: IO ()
main = do
  putStrLn "SALDO ➟ GF converter\n"
  args <- getArgs
  when (null args) $ fail "Please specify file to extract"
  let path = head args
  parser <- case takeExtension path of
    ".xml" -> return XML.parseDict
    ".json" -> return JSON.parseDict
    _ -> fail "Unsupported file extension"
  putStrLn $ "Parsing " ++ path
  mlex <- parser path
  case mlex of
    Nothing -> fail $ "Error parsing " ++ path
    Just lexi -> do
      printf "Parsed %d entries\n" (M.size lexi)
      let parts = splitMap chunkSize lexi
      doExtract parts 0

-- | Split map into smaller maps of size n
splitMap :: Ord k => Int -> M.Map k a -> [M.Map k a]
splitMap n mp =
  let
    ls = M.toList mp -- :: [(k,a)]
    lss = splitList n ls -- :: [[(k,a)]]
  in
    map M.fromList lss

-- | Split list into chunks of size n
splitList :: Int -> [a] -> [[a]]
splitList _ []  = []
splitList n xs = part:splitList n rest
  where
    (part,rest) = splitAt n xs
