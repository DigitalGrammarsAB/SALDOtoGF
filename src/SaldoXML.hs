{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | Parse SALDO XML format into internal datatype
-- Identical API to SaldoJSON

module SaldoXML (parseDict) where

import Common

import Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.XML.Light

mkQName :: String -> QName
mkQName s = QName s Nothing Nothing

parseDict :: FilePath -> IO (Maybe Lex)
parseDict d = do
  mroot <- parseXMLDoc <$> TIO.readFile d
  case mroot of
    Nothing -> putErrLn "Cannot parse XML file" >> return Nothing
    Just root -> do
      let lexentries = findElements (mkQName "LexicalEntry") root :: [Element]
      return $ Just $ M.fromList $ mapMaybe parseEntry lexentries
  where
    parseEntry :: Element -> Maybe (Text,Entry)
    parseEntry le = do
      lemma <- findChild (mkQName "Lemma") le
      fr <- findChild (mkQName "FormRepresentation") lemma
      lemgram <- getAttVal "lemgram" fr
      pos <- getAttVal "partOfSpeech" fr
      let table = parseTable le
      let entry = E pos table
      return (lemgram, entry)

    parseTable :: Element -> Table -- :: [(Text,Text)]
    parseTable le =
      let childs = findChildren (mkQName "WordForm") le :: [Element]
      in catMaybes $ flip map childs $ \wf -> do -- :: Maybe (Text,Text)
        writtenForm <- getAttVal "writtenForm" wf
        msd <- getAttVal "msd" wf
        return (msd, writtenForm)

-- | Find the child which matches "att" and return "val"
-- if element =
-- <FormRepresentation>
--  <feat att="writtenForm" val="kille"/>
--  <feat att="lemgram" val="kille..nn.1"/>
--  <feat att="partOfSpeech" val="nn"/>
--  <feat att="paradigm" val="nn_2u_vinge"/>
--  <feat att="inherent" val="u"/>
-- </FormRepresentation>
-- getAttVal "paradigm" element == Just "nn_2u_vinge"
getAttVal :: String -> Element -> Maybe Text
getAttVal att el = do
  feat <- filterChild (\el -> findAttr (mkQName "att") el == Just att) el
  T.pack <$> findAttr (mkQName "val") feat
