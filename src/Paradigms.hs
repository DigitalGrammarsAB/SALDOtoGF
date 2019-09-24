{-# LANGUAGE OverloadedStrings #-}

--  | Mapping from SALDO categories/params to GF Resource Library

module Paradigms where

import Common
import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow (first)

-- all word classes that should be imported should be listed here.
catMap :: [(Text, Text, ParamMap, (Text, Text), ParadigmList)]
catMap  =
  [ ("ab", "Adv", advParamMap,  ("mkAdv",""), advParadigmList)
  , ("av",   "A", adjParamMap,  ("mkA",""), adjParadigmList)
  , ("vb",   "V", verbParamMap, ("mkV",""), verbParadigmList)
  , ("nn",   "N", nounParamMap, ("mkN",""), nounParadigmList)
  -- particles were V2. Why? -"dirV2 (partV (mkV",")"
  -- VR should not be V2 either.
--  , ("vbm", "VR", verbRParamMap, ("reflV (mkV",")"), verbRParadigmList)
--  , ("vbm", "VP", verbPParamMap, ("partV (mkV",""), verbPParadigmList)
  ]

-- | Find relevant param map for SALDO cat
getParamMap :: Text -> Maybe ParamMap
getParamMap cat =
  case [mp | (sl_cat,_,mp,_,_) <- catMap, sl_cat == cat] of
    [] -> Nothing
    mp:_ -> Just mp

-- | Find relevant param map for GF cat (fails on error)
getParamMapGF :: Text -> ParamMap
getParamMapGF cat = head' "getParamMap" [mp | (_,gf_cat,mp,_,_) <- catMap, gf_cat == cat]

-- For prepositions, not run automatically
prepCatMap :: [(Text, Text, [(Text, Text)], (Text, Text), ParadigmList)]
prepCatMap = [("pp", "Prep", [("invar","s")],("mkPrep",""),[("mkPrep",["s"],"")])]

advParamMap :: ParamMap
advParamMap =
  [("pos", ["s"])
  ,("invar",["s"]) -- is invar needed?
  ]

advParadigmList :: ParadigmList
advParadigmList =
  [("mkAdv", ["s"], "")
  ]

a1 = "s (AF (APosit (Strong (GSg Utr))) Nom)"
a2 = "s (AF (APosit (Strong (GSg Utr))) Gen)"
a3 = "s (AF (APosit (Strong (GSg Neutr))) Nom)"
a4 = "s (AF (APosit (Strong (GSg Neutr))) Gen)"
a5 = "s (AF (APosit (Strong GPl)) Nom)"
a6 = "s (AF (APosit (Strong GPl)) Gen)"
a7 = "s (AF (APosit (Weak Sg)) Nom)"
a8 = "s (AF (APosit (Weak Sg)) Gen)"
a9 = "s (AF (APosit (Weak Pl)) Nom)"
a10 = "s (AF (APosit (Weak Pl)) Gen)"
a11 = "s (AF ACompar Nom)"
a12 = "s (AF ACompar Gen)"
a13 = "s (AF (ASuperl SupStrong) Nom)"
a14 = "s (AF (ASuperl SupStrong) Gen)"
a15 = "s (AF (ASuperl SupWeak) Nom)"
a16 = "s (AF (ASuperl SupWeak) Gen)"

adjParamMap :: ParamMap
adjParamMap =
  [("pos indef sg u nom",      [a1] )
  ,("pos indef sg u gen",      [a2] )
  ,("pos indef sg n nom",      [a3] )
  ,("pos indef sg n gen",      [a4] )
  ,("pos indef pl nom",        [a5] )
  ,("pos indef pl gen",        [a6] )
  ,("pos def sg no_masc nom",  [a7] )
  ,("pos def sg no_masc gen",  [a8] )
  ,("pos def pl nom",          [a9] )
  ,("pos def pl gen",          [a10])
  ,("komp nom",                [a11])
  ,("komp gen",                [a12])
  ,("super indef nom",         [a13])
  ,("super indef gen",         [a14])
  ,("super def no_masc nom",   [a15])
  ,("super def no_masc gen",   [a16])
  ]

adjParadigmList :: ParadigmList
adjParadigmList =
  [ ("mkA", [a1], "") , ("mkA", [a1, a3], "")
  , ("mkA", [a1, a11, a13], "")
  , ("mkA", [a1, a3 , a5, a11 , a13], "")
  , ("mkA", [a1, a3 , a5, a11 , a13], "")
  , ("mkA", [a1, a3 , a7, a5 , a11, a13, a15], "")
  , ("mk3A", [a1, a3, a5], "")
  ]

v1  = "s (VF (VPres Act))"
v2  = "s (VF (VPres Pass))"
v3  = "s (VF (VPret Act))"
v4  = "s (VF (VPret Pass))"
v5  = "s (VF (VImper Act))"
v5a  = "s (VF (VImper Pass))"
v6  = "s (VI (VInfin Act))"
v7  = "s (VI (VInfin Pass))"
v8  = "s (VI (VSupin Act))"
v9  = "s (VI (VSupin Pass))"
v10 = "s (VI (VPtPret (Strong (GSg Utr)) Nom))"
v11 = "s (VI (VPtPret (Strong (GSg Utr)) Gen))"
v12 = "s (VI (VPtPret (Strong (GSg Neutr)) Nom))"
v13 = "s (VI (VPtPret (Strong (GSg Neutr)) Gen))"
v14 = "s (VI (VPtPret (Strong GPl) Nom))"
v15 = "s (VI (VPtPret (Strong GPl) Gen))"
v16 = "s (VI (VPtPret (Weak Sg) Nom))"
v17 = "s (VI (VPtPret (Weak Sg) Gen))"
v18 = "s (VI (VPtPret (Weak Pl) Nom))"
v19 = "s (VI (VPtPret (Weak Pl) Gen))"

verbParamMap :: ParamMap
--"s (VF (VImper Pass))")   "part")
verbParamMap =
  [("pres ind aktiv",               [v1] )
  ,("pres ind s-form",              [v2] )
  ,("pret ind aktiv",               [v3] )
  ,("pret ind s-form",              [v4] )
  ,("imper",                        [v5,v5a] )
  ,("inf aktiv",                    [v6] )
  ,("inf s-form",                   [v7] )
  ,("sup aktiv",                    [v8] )
  ,("sup s-form",                   [v9] )
  ,("pret_part indef sg u nom",     [v10])
  ,("pret_part indef sg u gen",     [v11])
  ,("pret_part indef sg n nom",     [v12])
  ,("pret_part indef sg n gen",     [v13])
  ,("pret_part indef pl nom",       [v14])
  ,("pret_part indef pl gen",       [v15])
  ,("pret_part def sg no_masc nom", [v16])
  ,("pret_part def sg no_masc gen", [v17])
  ,("pret_part def pl nom",         [v18])
  ,("pret_part def pl gen",         [v19])
  ]

verbParadigmList :: ParadigmList
verbParadigmList =
  [ ("mkV", [v1], "")
  , ("mkV", [v6, v3, v8], "")
  , ("mkV", [v6, v1, v5, v3, v8, v10], "")
  ]

-- could use normal verbParamMap if we are sure it is a preposition,
-- and will look the same in all paradims
verbPParamMap :: ParamMap
verbPParamMap = map (first (T.append " 1:1-2")) verbParamMap
              ++map (\(a,b) -> (T.append a " 1:2-2",["part"])) verbParamMap

verbPParadigmList :: ParadigmList
verbPParadigmList =
  [ ("", [v1], "" )
  , ("", [v6, v3, v8], "")
  , ("", [v6, v1, v5, v3, v8, v10], "")
  ]

verbRParamMap :: ParamMap
verbRParamMap = map (first (T.append " 1:1-2")) verbParamMap

verbRParadigmList :: ParadigmList
verbRParadigmList =
  [ ("", [v1],  "")
  , ("", [v6, v3, v8], "")
  , ("", [v6, v1, v5, v3, v8, v10], "")
  ]

n1 = "s Sg Indef Nom"
n2 = "s Sg Indef Gen"
n3 = "s Sg Def Nom"
n4 = "s Sg Def Gen"
n5 = "s Pl Indef Nom"
n6 = "s Pl Indef Gen"
n7 = "s Pl Def Nom"
n8 = "s Pl Def Gen"

nounParamMap :: ParamMap
nounParamMap =
  [ ("sg indef nom", [n1])
  , ("sg indef gen", [n2])
  , ("sg def nom",   [n3])
  , ("sg def gen",   [n4])
  , ("pl indef nom", [n5])
  , ("pl indef gen", [n6])
  , ("pl def nom",   [n7])
  , ("pl def gen",   [n8])
  ]

nounParadigmList :: ParadigmList
nounParadigmList =
  [ ("mkN", [n1], "")
  , ("mkN", [n1], "utrum")
  , ("mkN", [n1], "neutrum")
  , ("mkN", [n1, n5], "")
  , ("mkN", [n1, n3, n5, n7], "")
  , ("mkN", [n1, n3, n1, n3], "") -- no plural
  ]

-- -- | all Swedish prepostions according to Wikipedia
-- -- http://sv.wiktionary.org/wiki/Kategori:Svenska/Prepositioner
-- preps :: [String]
-- preps =
--   ["à"
--   ,"af"
--   ,"an"
--   ,"angående"
--   ,"apropå"
--   ,"av"
--   ,"bak"
--   ,"bakanför"
--   ,"bakför"
--   ,"bakom"
--   ,"bland"
--   ,"bortanför"
--   ,"bortom"
--   ,"bredvid"
--   ,"efter"
--   ,"emellan"
--   ,"enligt"
--   ,"exklusive"
--   ,"framanför"
--   ,"framför"
--   ,"framom"
--   ,"från"
--   ,"för"
--   ,"före"
--   ,"förutom"
--   ,"genom"
--   ,"gentemot"
--   ,"givet"
--   ,"hinsides"
--   ,"hitom"
--   ,"hos"
--   ,"i"
--   ,"ifrån"
--   ,"igenom"
--   ,"ikring"
--   ,"in" -- in is not in the original list, but needed
--   ,"inifrån"
--   ,"inklusive"
--   ,"innan"
--   ,"innanför"
--   ,"inom"
--   ,"intill"
--   ,"inuti"
--   ,"invid"
--   ,"jämlikt"
--   ,"jämte"
--   ,"kontra"
--   ,"kring"
--   ,"längs"
--   ,"längsefter"
--   ,"med"
--   ,"medelst"
--   ,"mellan"
--   ,"mittemellan"
--   ,"mittimellan"
--   ,"mot"
--   ,"mä"
--   ,"oavsett"
--   ,"om"
--   ,"ovan"
--   ,"ovanför"
--   ,"ovanpå"
--   ,"per"
--   ,"på"
--   ,"runt"
--   ,"runtomkring"
--   ,"sedan"
--   ,"som"
--   ,"te"
--   ,"till"
--   ,"tillika"
--   ,"tills"
--   ,"trots"
--   ,"under"
--   ,"undör"
--   ,"uppför"
--   ,"uppå"
--   ,"ur"
--   ,"ut"-- in is not in the original list, but needed
--   ,"utan"
--   ,"utanför"
--   ,"utanpå"
--   ,"utav"
--   ,"uti"
--   ,"utifrån"
--   ,"utom"
--   ,"utur"
--   ,"utöver"
--   ,"via"
--   ,"vid"
--   ,"visavi"
--   ,"än"
--   ,"å"
--   ,"åt"
--   ,"öfver"
--   ,"öfwer"
--   ,"över"
--   ]
