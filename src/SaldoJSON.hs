{-# LANGUAGE OverloadedStrings #-}

-- | Parse SALDO JSON format into internal datatype
-- Identical API to SaldoXML

module SaldoJSON (parseDict) where

import Common

import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Text (Text)
import Data.Aeson

-- | Corresponds to one line in JSON format
-- e.g.: {"word":"dväljes","head":"dväljas","pos":"vb","param":"pres ind s-form","inhs":[],"id":"dväljas..vb.1","p":"vb_vs_dväljas","attr":"0"}
data Line = L
  { lWord :: Text -- ^ surface form,
  , lHead :: Text -- ^ lemma
  , lPOS :: Text  -- ^ part of speech
  , lParam :: Text -- ^ features
  -- , lInhs :: [Text]
  , lId :: Text -- ^ concept ID
  , lP :: Text -- ^ paradigm
  -- , lAttr :: Text -- ^ attributes
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
  { ePOS = lPOS l
  , eTable = [(lParam l, lWord l)]
  }

combineEntries :: Entry -> Entry -> Entry
combineEntries a b | ePOS a /= ePOS b = error "Cannot combine entries"
combineEntries a b = E
  { ePOS = ePOS a
  , eTable = eTable a ++ eTable b
  }
