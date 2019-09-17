
-- | Shared data types and functions

module Common where

import qualified Data.Map as M
import Data.Text (Text)
import System.IO (hPutStrLn, stderr)

type Lex = M.Map Text Entry -- key is lemgram ID

type Table = [(Text,Text)]

data Entry = E
  { pos :: Text
  , table :: Table -- morphological tags to surface form: ("sg def gen" ,"killens")
  } deriving Show

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

nl :: IO ()
nl = putStrLn ""
