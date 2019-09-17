{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | Parse SALDO XML format into internal datatype
-- Identical API to SaldoJSON

module SaldoXML (Lex, Entry(..), parseDict) where

import Common

import qualified Data.Map as M
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
    Just root ->
      return $ Just $ M.fromList
        [ (lemgram, entry)
        | le <- findElements (mkQName "LexicalEntry") root
        , let Just lemma = findChild (mkQName "Lemma") le
        , let Just fr = findChild (mkQName "FormRepresentation") lemma
        , let Just lemgram = getAttVal "lemgram" fr
        , let Just pos = getAttVal "partOfSpeech" fr
        , let table =
                [ (writtenForm, msd)
                | wf <- findChildren (mkQName "WordForm") le
                , let Just writtenForm = getAttVal "writtenForm" wf
                , let Just msd = getAttVal "msd" wf -- TODO seperate
                ]
        , let entry = E pos table
        ]

-- | Find the "feat" which matches "att" and return "val"
getAttVal :: String -> Element -> Maybe Text
getAttVal att el = do
  feat <- filterChild (\el -> findAttr (mkQName "att") el == Just att) el
  T.pack <$> findAttr (mkQName "val") feat

-- -- parseDictList :: String -> IO (Maybe LexList)
-- -- parseDictList d = listToMaybe <$> mainF xpLexList d return
--
-- xpLex = xpElem "LexicalResource"
--           $ xpElem "Lexicon"
--              $ xpWrap (M.fromList,M.toList)
--                 $ xpList xpEntry
--
-- -- xpLexList = xpElem "Lexicon"
-- --             $ xpList xpEntry
--
-- xpEntry :: PU (String,Entry)
-- xpEntry = xpElem "LexicalEntry"
--             $ xpWrap ((\(lem,_,_,_,pos,_,table) ->
--                          --(nameWord (gf,pos),E (pack pos) table))
--                          (lem,E (pack pos) table))
--                      ,(\(w,E p t)  -> (w,Just w,unnameWord w,w,unpack p,"-",t)))
--             $ xp7Tuple
--               (xpElem "lem" xpText)
--               xpSaldos
--               (xpElem "gf" xpText)
--               (xpElem "p" xpText)
--               (xpElem "pos" xpText)
--               (xpElem "inhs" xpText)
--               (xpElem "table" xpTable)
--
-- xpTable :: PU [(ByteString,String)]
-- xpTable = xpList $ xpElem "form"
--           $ xpWrap (first pack,first unpack)
--           $ xpPair
--            (xpElem "param" xpText)
--            (xpElem "wf"    xpText)
--
-- -- very ugly indeed, how to solve this?
-- xpSaldos :: PU (Maybe String)
-- xpSaldos = xpWrap (\(x,_,_,_,_, _,_,_,_,_, _,_,_,_,_, _,_,_,_,_) -> x
--                  , \x -> (x,x,x,x,x ,x,x,x,x,x, x,x,x,x,x ,x,x,x,x,x))
--            $ xp20Tuple
--              maybeSaldo maybeSaldo maybeSaldo maybeSaldo maybeSaldo
--              maybeSaldo maybeSaldo maybeSaldo maybeSaldo maybeSaldo
--              maybeSaldo maybeSaldo maybeSaldo maybeSaldo maybeSaldo
--              maybeSaldo maybeSaldo maybeSaldo maybeSaldo maybeSaldo
--
--   where maybeSaldo = xpOption $ xpElem "saldo" xpText
--
-- nameWord :: (String,String) -> String
-- nameWord (name,tag) = name++"_"++toGF tag
-- unnameWord = takeWhile (/='_')
-- toGF :: String -> String
-- toGF "av" = "A"
-- toGF "nn" = "N"
-- toGF "vb" = "V"
-- toGF "ab" = "Adv"
-- toGF x    = x
--
--
-- mainF xpFunc src f =
--   runX (xunpickleDocument xpFunc [withInputEncoding utf8
--                                      , withRemoveWS yes] src
--         >>> arrIO f) --(return . f))


  {-
  <LexicalEntry>
    <lem>fort..ab.1</lem>
    <saldo>fort..1</saldo>
    <gf>fort</gf>
    <p>ab_1_fort</p>
    <pos>ab</pos>
    <inhs>-</inhs>
    <table>
     <form><param>pos</param><wf>fort</wf></form>
     <form><param>komp</param><wf>fortare</wf></form>
     <form><param>super</param><wf>fortast</wf></form>
     <form><param>c</param><wf>fort</wf></form>
     <form><param>c</param><wf>fort-</wf></form>
     <form><param>sms</param><wf>fort-</wf></form>

    </table>
   </LexicalEntry>
-}
{-
data Noun = NDef { sg_indef_nom, sg_indef_gen, sg_def_nom, sg_def_gen,
                   pl_indef_nom, pl_indef_gen, pl_def_nom, pl_def_gen
                   :: String}

data Adj = ADef {pos_indef_sg_u_nom, pos_indef_sg_u_gen,
                 pos_indef_sg_n_nom, pos_indef_sg_n_gen,
                 pos_indef_pl_nom, pos_indef_pl_gen,
                 pos_def_sg_no_masc_nom, pos_def_sg_no_masc_gen,
                 pos_def_sg_masc_nom, pos_def_sg_masc_gen,
                 pos_def_pl_nom, pos_def_pl_gen
                 :: String}

data Verb = VDef { pres_ind_aktiv, pres_ind_sform, pres_konj_aktiv,
                   pres_konj_sform, pret_ind_aktiv, pret_ind_sform,
                   pret_konj_aktiv, pret_konj_sform,imper, inf_aktiv,
                   inf_sform, sup_aktiv, sup_sform, pres_part_nom,
                   pres_part_gen :: String}
-}
