{-# LANGUAGE OverloadedStrings #-}

-- | Parse SALDO JSON format into internal datatype
-- Drop-in replacement for SaldoXML

module SaldoJSON where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Builder as BSLB
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Aeson

type Lex = M.Map String Entry
-- type LexList =  [(String,Entry)]
data Entry = E {pos :: BSL.ByteString, table :: [(BSL.ByteString,String)]}
  deriving Show

-- | Corresponds to one line in JSON format
-- e.g.: {"word":"dväljes","head":"dväljas","pos":"vb","param":"pres ind s-form","inhs":[],"id":"dväljas..vb.1","p":"vb_vs_dväljas","attr":"0"}
data Line = L
  { lWord :: String -- ^ surface form,
  , lHead :: String -- ^ lemma
  , lPOS :: String  -- ^ part of speech
  , lParam :: String -- ^ features
  -- , lInhs :: [String]
  , lId :: String -- ^ concept ID
  , lP :: String -- ^ paradigm
  -- , lAttr :: String -- ^ attributes
  }
  deriving Show

instance FromJSON Line where
  parseJSON = withObject "Line" $ \v -> L
    <$> v .: "word"
    <*> v .: "head"
    <*> v .: "pos"
    <*> v .: "param"
    -- <*> v .: "inhs"
    <*> v .: "id"
    <*> v .: "p"
    -- <*> v .: "attr"

parseDict :: FilePath -> IO (Maybe Lex)
parseDict path = do
  s <- BSL.readFile path
  let
    tryDecode :: BSL.ByteString -> Either String Line
    tryDecode s =
      case decode s of
        Just x  -> Right x
        Nothing -> Left $ "Cannot parse Line from: " ++ show s
    elns = map tryDecode (C8.lines s) :: [Either String Line]
  sequence_ [ putStrLn err | Left err <- elns ]
  let
    lns = [ (lid, mkEntry line) | Right line@L{lId = lid} <- elns ]
    lx = M.fromListWith combineEntries lns
  return $ Just lx

mkEntry :: Line -> Entry
mkEntry l = E
  { pos = toBS (lPOS l)
  , table = [(toBS (lParam l), lWord l)]
  }

combineEntries :: Entry -> Entry -> Entry
combineEntries a b | pos a /= pos b = error "Cannot combine entries"
combineEntries a b = E
  { pos = pos a
  , table = table a ++ table b
  }

toBS :: String -> BSL.ByteString
toBS = BSLB.toLazyByteString . BSLB.stringUtf8
