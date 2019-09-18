
-- | Shared data types and functions

module Common where

import qualified Data.Map as M
import Data.Text (Text)
import System.IO (hPutStrLn, stderr)

type Lex = M.Map Text Entry -- key is lemgram ID

type Table = [(Text,Text)]

data Entry = E
  { ePOS :: Text
  , eTable :: Table -- morphological tags to surface form: ("sg def gen" ,"killens")
  } deriving Show

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

nl :: IO ()
nl = putStrLn ""

writeReportFile :: FilePath -> String -> IO ()
writeReportFile path contents = do
  putStrLn $ "Writing " ++ path
  writeFile path contents
