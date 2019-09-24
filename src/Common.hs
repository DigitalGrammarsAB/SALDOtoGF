
-- | Shared data types and functions

module Common where

import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Text (Text)
import Text.Printf
import System.IO (hPutStrLn, stderr)

-- Extraction

type Lex = M.Map Text Entry -- key is lemgram ID

type Table = [(Text,Text)]

data Entry = E
  { ePOS :: Text
  , eTable :: Table -- morphological tags to surface form: ("sg def gen" ,"killens")
  } deriving (Show, Read)

showLex :: Lex -> String
showLex lx = L.intercalate "\n"
  [ printf "%s (%s)\n" id (ePOS entry)
  ++ showListIndent 2 (eTable entry)
  | (id,entry) <- M.toList lx
  ]

-- Processing

type ParamMap = [(Text, [Text])]

type ParadigmList = [(Text, [Text], Text)]

-- Formatting

showListIndent :: Show a => Int -> [a] -> String
showListIndent n x = tab ++ L.intercalate ("\n" ++ tab) (map show x)
  where tab = L.replicate n ' '

-- Lists

-- | Find all values matching key in non-unique key-value list
-- lookup' a [(a,1),(b,2),(a,3)] == [1,3]
lookup' :: Eq a => a -> [(a,b)] -> [b]
lookup' a  =  map snd . filter ((== a) . fst)

-- | Head which reports context on failure
head' :: String -> [a] ->  a
head' s []     = error $ "Error in head in "++s
head' _ (x:xs) = x

-- IO

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

nl :: IO ()
nl = putStrLn ""

writeReportFile :: FilePath -> String -> IO ()
writeReportFile path contents = do
  putStrLn $ "Writing " ++ path
  writeFile path contents
