{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Main extraction algorithm

module Saldoer (doExtract) where

import System.IO (hClose, openTempFile, stdout, hSetBuffering, BufferMode(..))
import System.Process (rawSystem)
import System.Exit (ExitCode(..))
import System.Directory (removeFile, copyFile)
import System.FilePath ((</>),(<.>))

import Data.Char (isDigit,isAlpha)
import Data.Maybe (catMaybes,mapMaybe)
import qualified Data.Map as Map
import Data.List (delete, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
-- import Data.ByteString.Lazy (ByteString)
-- import Data.ByteString.Lazy.Char8 (pack)
import Text.Printf

import Control.Monad (when, unless, zipWithM)
import Control.Monad.Trans (lift)
import Control.Monad.State (StateT(..), modify, gets, execStateT)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Arrow (first)
import Control.Exception (onException)

import qualified PGF

import Common
-- import SaldoJSON (Entry(..), Lex, parseDict)
import SaldoXML (Entry(..), Lex, parseDict)

-----------------------------------------------------------------------------
-- Directories

genDir :: FilePath
genDir = "generate"

buildDir :: FilePath
buildDir = "build"

logDir :: FilePath
logDir = "logs"

-----------------------------------------------------------------------------
-- Types

type Convert = StateT CState (ExceptT String IO)

data CState = CS
  { errs :: [String]
  , msg :: [String]
  , retries :: [GrammarInfo]
  , pgf :: FilePath
  , ok :: [GrammarInfo]
  , saldo :: Lex
  , changes :: Int
  , dead :: [Text]
  , tmps :: [FilePath]
  , partNo :: Int
  , selection :: Maybe [Text]
  , name :: String
}

data GrammarInfo = G
  { lemma :: Text
  , pos :: Text
  , forms :: [[Text]]
  , extra :: Text
  , functions :: (Text,Text)
  , paradigms :: Paradigm
  } deriving Show

type Paradigm = [(Text,[Text],Text)]

instance Eq GrammarInfo where
  -- checks equality on the name only, in order to simplify deletion from retries-list
  g1 == g2 = lemma g1 == lemma g2

-----------------------------------------------------------------------------

-- | Main entry point: extract all parts and write final GF modules
doExtract :: [FilePath] -> Int -> IO ()
doExtract fpaths skip = do
  let name = "Tot"
  entriess <- zipWithM (extract Nothing name) fpaths [skip..]
  let entriesSorted = sortOn lemma (concat entriess)
  writeFile (genDir </> "DictSweAbs.gf") $ absHeader "DictSweAbs" ++ concatMap showAbs entriesSorted ++ "}\n"
  writeFile (genDir </> "DictSwe.gf") $ concHeader "DictSwe" "DictSweAbs" ++ concatMap showCnc entriesSorted ++ "}\n"

-- | Extract individual part
extract :: Maybe [Text] -> String -> FilePath -> Int -> IO [GrammarInfo]
extract select name inputFile n  = do
  hSetBuffering stdout NoBuffering
  putStr $ "Reading "++inputFile++" ... "
  res   <- parseDict inputFile
  saldo <- case res of
             Just s   -> return s
             Nothing  -> fail $ "cannot read "++inputFile
  nl

  mst <- runExceptT $ execStateT (createGF saldo
                    >> compileGF
                    >> loop
                    >> printGFFinal
                    >> cleanUp)  (initState n select name)
  (entries,errs) <- case mst of
             Left er  -> return ([],er)
             Right st -> do
                 let ms   =  "\n Messages:\n "++ unlines (msg st)
                 let fails =  "\n Failing:\n " ++ show (retries st) ++ T.unpack (T.unlines (dead st))
                 writeFile (logFile "messages" n) ms
                 writeFile (logFile "fail" n) fails
                 -- appendCode name $ ok st
                 return (ok st, unlines (errs st))
  writeFile (logFile "errors" n)  $ "\n Errors:\n "  ++ errs
  return entries

  where
    logFile :: String -> Int -> FilePath
    logFile s n = logDir </> s++show n <.> "txt"

    loop = do
      updateGF
      compileGF
      changes <- gets changes
      unless (changes == 0) loop

-------------------------------------------------------------------
-- Bootstrap initial version of GF
-------------------------------------------------------------------

createGF :: Lex -> Convert ()
createGF sal = do
  io $ putStr "Creating GF source for SALDO ..."
  mapM_ findGrammar (Map.toList sal)
  todo <- gets retries
  when (null todo) $ fail "No words from this section. Skipping"
  modify $ \s -> s {saldo = sal}

  printGF
  report "Lexicon created"
  io nl

findGrammar :: (Text,Entry) -> Convert ()
findGrammar (id,E pos table) =  do
  rest <- gets selection
  let keep = maybe True (id `elem`) rest
  when keep $ do
    let xs =
          [ (gf_cat,f,paradigms)
          | (fm_cat,gf_cat,_,f,paradigms) <- catMap
          , fm_cat == pos
          , okCat gf_cat id
          ]
    if null xs
    then do
      tellFailing (printf "don't accept pos %s, word '%s' rejected." pos id)
      isDead id
    else do
      let cnc =
            [ G id gf_cat [[snd $ head' "createGF" table]] "" (f,findA gf_cat (T.append id f')) paradigms
            | (gf_cat,(f,f'),paradigms) <- xs]
      modify $ \s -> s {retries = cnc++retries s}

--- particles can be prepositions (hitta på), adverbs (åka hem), nouns (åka hem)...
--  we first check for reflexives, and accept the rest as particles.

okCat :: Text -> Text -> Bool
okCat "VP" = hasPart
okCat "VR" = isRefl
okCat _    = const True

findA :: Text -> Text -> Text
findA "VP" w =  T.concat [")\"", part, "\""] where part = findsndWord w
findA _  _ = ""
--w | part `elem` preps = ")\""++part++"\"" --paranthesis to close mkV
        -- | otherwise         = ""

hasPart :: Text -> Bool
hasPart = (\x -> T.all isAlpha x && x /= "sig") . findsndWord

-- hasPrep :: Text -> Bool
-- hasPrep = (`elem` preps) . findsndWord

isRefl :: Text -> Bool
isRefl  = (=="sig") . findsndWord

-- extracts all words except for the first
findsndWord :: Text -> Text
findsndWord = T.drop 1 . T.takeWhile (/='.') . T.dropWhile (/='_')

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

-------------------------------------------------------------------
-- Compare the current GF version with SALDO
-------------------------------------------------------------------

updateGF :: Convert ()
updateGF = do
  report "will update"
  tmp_saldo <- gets pgf
  io $ putStr ("Reading "++tmp_saldo++" ... ")
  pgf <- io $ PGF.readPGF tmp_saldo
  io nl

  io $ putStr "Updating GF source for SALDO ... "
  cnc_file <- gets retries
  modify $ \s -> s {changes = 0, retries = []}
  report $ "updating "++ unlines (map show cnc_file)
  mapM_ (check pgf) cnc_file
  printGF
  c <- gets changes
  io nl
  let update = "updated "++show c++" words"
  io $ putStrLn update
  report update

check :: PGF.PGF -> GrammarInfo -> Convert ()
check gf entry@(G id cat lemmas _ _ _) = do
  saldo <- gets saldo
  case Map.lookup id saldo of
       Just (E p t) -> checkWord gf t entry
       Nothing      -> do tellFailing (printf "unknown id in SALDO: %s" id)
                          isDead id

packTuple :: (String,String) -> (Text,Text)
packTuple (a,b) = (T.pack a, T.pack b)

checkWord :: PGF.PGF -> Table -> GrammarInfo -> Convert ()
checkWord gf t entry@(G id cat lemmas _ _ _) = do
  langName <- getLangName
  let
    gf_t = map packTuple $ concat $ PGF.tabularLinearizes gf (read langName) (read (mkGFName id cat))
    paramMap = head' "check" [map | (_,gf_cat,map,_,_) <- catMap, gf_cat == cat]
  checkForms paramMap t gf_t entry

checkForms :: [(Text, [Text])] -> Table -> Table -> GrammarInfo -> Convert ()
checkForms paramMap fm_t gf_t entry@(G id cat lemmas _ _  _)
  | null diffs = accept entry
  | otherwise  = do c <- gets changes
                    modify $ \s -> s {changes = c+1}
                    report $ printf "redo word %s" id
                    report (show [[(lookup f fm_t,lookup g' gf_t) | g' <- g ] | (f,g) <- paramMap])
                    getNextLemma $!  entry
  where
    diffs =
      [ (fm_p,fm_v,gf_p,gf_v)
      | (fm_p,gf_p) <- paramMap
      , fm_vs      <- [lookup' fm_p fm_t]
      , let gf_v  = mapMaybe (`lookup` gf_t) gf_p
      , Just fm_v  <- [isDiff gf_v fm_vs]
      ]
   -- if there is no information about the form in saldo, we chose to accept it
    isDiff  _ [] = Nothing
    isDiff ys xs | any (`elem` xs) ys = Nothing
                 | otherwise   = Just $ head xs

    getNextLemma x@(G id cat lemmas _ _ []) = do
        tellFailing (printf "No more paradigms to choose from: %s" id)
        isDead id
    getNextLemma entry@(G id cat lemmas b f ((pre,xs,a):ps)) = do
        report $ printf "working on %s" id
        report $ printf "next paradigm: %s" (show xs)
        report $ printf "to choose from: %s" (show ps)
        forms <- mapM getLemma xs
        if Nothing `elem` forms
        -- to do: if the comparative forms for an adjective doesn't exist, add compounda
           then do report (printf "all forms for %s not found" id)
                   getNextLemma (G id cat lemmas b f ps)
           else replace (G id cat [catMaybes forms] a f ps)
      where
        getLemma :: Text -> Convert (Maybe Text)
        getLemma gf_p =
          case lookup fm_p fm_t of
            Just "" -> do
              report (printf "form %s was empty for %s" gf_p id)
              return Nothing
            Just fm_v ->
              return $ Just fm_v
            x -> do
              report (printf "form %s do not exist %s" gf_p id)
              return Nothing
          where
            fm_ps = [fm_p | (fm_p,gf_p') <- paramMap, gf_p `elem` gf_p']
            fm_p = head' (printf "getLemma %s. Word: %s" gf_p id) fm_ps

-- | Compile with GF
compileGF :: Convert ()
compileGF = do
  io $ putStrLn "Compiling GF ... "
  concName <- getCncName
  pgfName  <- getPGFName
  res <- io $ rawSystem "gf" ["--batch", "--make", "--output-dir", buildDir, concName, "+RTS", "-K64M"]
  case res of
    ExitSuccess      -> return ()
    ExitFailure code -> do
                   n <- gets partNo
                   io $ putStrLn "failed to create PGF.\nWill skip this section"
                   fail ("compiliation failed with error code "++show code
                         ++"\nPartition "++show n++" will be skipped")
  (fpath,h) <- io $ openTempFile "." "tmp.pgf"
  io $ hClose h
  io $ copyFile pgfName fpath
  addTemp fpath
  report "compilation done"
  setPGF fpath


-- | Generate GF identifier
mkGFName :: Text -> Text -> String
mkGFName id' cat = printf "%s_%c_%s" name num (toGFcat cat)
  where
    toGFcat "VR" = "V"
    toGFcat "VP" = "V"
    toGFcat  v   = v
    dash2us = T.replace "-" "_"
    pfxnum x = if isDigit (T.head x) then 'x' `T.cons` x else x -- don't start with a digit
    name =  pfxnum
          $ dash2us
          $ T.takeWhile (/= '.')
            id'
    num = T.last id'

-------------------------------------------------------------------
-- Mapping from SALDO categories/params to GF Resource Library
-------------------------------------------------------------------

-- all word classes that should be imported should be listed here.
catMap :: [(Text, Text, [(Text, [Text])], (Text, Text), [(Text, [Text], Text)])]
catMap  =
  [ ("ab", "Adv", advParamMap,  ("mkAdv",""), advParadigmList )
  , ("av",   "A", adjParamMap,  ("mkA",""), adjParadigmList )
  , ("vb",   "V", verbParamMap, ("mkV",""), verbParadigmList)
  , ("nn",   "N", nounParamMap, ("mkN",""), nounParadigmList)
  -- particles were V2. Why? -"dirV2 (partV (mkV",")"
  -- VR should not be V2 either.
--  , ("vbm", "VR", verbRParamMap, ("reflV (mkV",")"), verbRParadigmList)
--  , ("vbm", "VP", verbPParamMap, ("partV (mkV",""), verbPParadigmList)
  ]

-- For prepositions, not run automatically
prepCatMap :: [(Text, Text, [(Text, Text)], (Text, Text), [(Text, [Text], Text)])]
prepCatMap =  [("pp", "Prep", [("invar","s")],("mkPrep",""),[("mkPrep",["s"],"")])]

advParamMap :: [(Text,[Text])]
advParamMap =
  [("pos", ["s"]),("invar",["s"])] -- is invar needed?

advParadigmList :: [(Text, [Text], Text)]
advParadigmList =
  [("mkAdv", ["s"], "") ]

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

adjParamMap :: [(Text, [Text])]
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

adjParadigmList :: [(Text, [Text], Text)]
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

verbParamMap :: [(Text, [Text])]
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

verbParadigmList :: [(Text, [Text], Text)]
verbParadigmList =
  [ ("mkV", [v1], "")
  , ("mkV", [v6, v3, v8], "")
  , ("mkV", [v6, v1, v5, v3, v8, v10], "")
  ]

-- could use normal verbParamMap if we are sure it is a preposition,
-- and will look the same in all paradims
verbPParamMap :: [(Text, [Text])]
verbPParamMap = map (first (T.append " 1:1-2")) verbParamMap
              ++map (\(a,b) -> (T.append a " 1:2-2",["part"])) verbParamMap

verbPParadigmList :: [(Text, [Text], Text)]
verbPParadigmList =
  [ ("", [v1], "" )
  , ("", [v6, v3, v8], "")
  , ("", [v6, v1, v5, v3, v8, v10], "")
  ]

verbRParamMap :: [(Text, [Text])]
verbRParamMap = map (first (T.append " 1:1-2")) verbParamMap

verbRParadigmList :: [(Text, [Text], Text)]
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

nounParamMap :: [(Text, [Text])]
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

nounParadigmList :: [(Text, [Text], Text)]
nounParadigmList =
  [ ("mkN", [n1], "")
  , ("mkN", [n1], "utrum")
  , ("mkN", [n1], "neutrum")
  , ("mkN", [n1, n5], "")
  , ("mkN", [n1, n3, n5, n7], "")
  ]


-------------------------------------------------------------------
-- Dump GF code
-------------------------------------------------------------------

printGFFinal :: Convert ()
printGFFinal = do
  good <- gets ok
  num <- gets partNo
  io $ printGF' good (show num) "Fin"

printGF :: Convert ()
printGF = do
  new  <- gets retries
  good <- gets ok
  let entries = new++good
  num <- gets partNo
  nam <- gets name
  io $ printGF' entries (show num) nam

printGF' :: [GrammarInfo] -> String -> String -> IO ()
printGF' [] _ _ = putStrLn "no lemmas to write"
printGF' entries num name = do
  let
    sname = "saldo"++name++num
    absName = genDir </> sname <.> "gf"
    cncName = genDir </> sname ++ "Cnc" <.> "gf"
  writeFile  absName $
      absHeader sname ++
      concatMap showAbs entries ++
      "}\n"
  writeFile cncName $
      concHeader (sname++"Cnc") sname ++
      concatMap showCnc entries ++
      "}\n"

showAbs :: GrammarInfo -> String
showAbs (G id cat lemmas a _ paradigms) = printf "  %s : %s ; -- %s\n" (mkGFName id cat) (find cat) id
  where
    find "VR" = "V"
    find "VP" = "V"
    find x    = x

showCnc :: GrammarInfo -> String
showCnc (G id cat [[]] a _ paradigms) = printf "-- %s has not enough forms\n" (mkGFName id cat)
showCnc (G id cat lemmas a (mk,end) paradigms)
  = printf "  %s = %s %s%s%s ; -- %s\n" (mkGFName id cat) mk defs extra end id
 where
    defs = T.unwords [ if null lemma_v then "(variants {})" else T.unwords (map fnutta lemma_v) | lemma_v <- lemmas]
    extra = if T.null a then "" else T.concat [" ", a, " "]

    -- avoid putting extra fnutts on variants"
    -- wrong! how to handle this.. maybe won't occur again?
    fnutta :: Text -> Text
    fnutta x@"variant {}" = T.concat ["(", x, ")"]
    fnutta x = T.concat ["\"", x, "\""]

absHeader :: String -> String
absHeader nam = unlines
  [ "--# -path=.:abstract:alltenses/"
  , printf "abstract %s = Cat ** {" nam
  , ""
  , "fun"
  ]

concHeader :: String -> String -> String
concHeader cname aname = unlines
  [ "--# -path=.:swedish:scandinavian:abstract:common:alltenses"
  , printf "concrete %s of %s = CatSwe ** open ParadigmsSwe in {" cname aname
  , ""
  , "flags"
  , "  optimize=values ; coding=utf8 ;"
  , ""
  , "lin"
  ]

getCncName :: Convert String
getCncName = do
 n   <- gets partNo
 nam <- gets name
 return $ genDir </> "saldo"++nam++show n++"Cnc" <.> "gf"

getAbsName :: Convert String
getAbsName = do
 n   <- gets partNo
 nam <- gets name
 return $ genDir </> "saldo"++nam++show n <.> "gf"

getLangName :: Convert String
getLangName = do
 n   <- gets partNo
 nam <- gets name
 return $ "saldo"++nam++show n++"Cnc"

getPGFName :: Convert String
getPGFName = do
 n   <- gets partNo
 nam <- gets name
 return $ buildDir </> "saldo"++nam++show n <.> "pgf"

cleanUp :: Convert ()
cleanUp = do
  ts <- gets tmps
  io $ mapM_ (\t -> onException (removeFile t) (return ())) ts

-------------------------------------------------------------------
-- State
-------------------------------------------------------------------

initState :: Int -> Maybe [Text] -> String -> CState
initState n s name
  = CS {errs = [], msg = [], retries = []
        , pgf = "", ok = [] ,dead = []
        , saldo = Map.empty, changes = 0, tmps = []
        , partNo = n, selection = s, name = name }

setPGF :: FilePath -> Convert ()
setPGF f = modify $ \s -> s { pgf = f}

replace :: GrammarInfo -> Convert ()
replace gr = modify $ \s -> s {retries = gr : delete gr (retries s)} -- hehe, make nice?

-- add to dead , remove from retries
isDead :: Text -> Convert ()
isDead d = modify $ \s -> s {dead = d:dead s, retries = delete (emptyG d) (retries s)}  -- hehe, make nice?
  where emptyG d = G d "" [] "" ("","") []

--add to ok, remove from retries
accept :: GrammarInfo -> Convert ()
accept e = do report $ printf "accepting %s" (lemma e)
              modify $ \s -> s {retries = delete e (retries s), ok = e:ok s}
              r <- gets retries
              report $ printf "deleted %s from retries. result: %s" (lemma e) (show (e `elem` r))

setPartNo :: Int -> Convert ()
setPartNo n = modify $ \s -> s {partNo = n}

addTemp :: FilePath -> Convert ()
addTemp f = modify $ \s -> s {tmps = f:tmps s}

-------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------

io :: IO a -> Convert a
io = lift . lift

report :: String -> Convert ()
report x = modify $ \s  -> s {msg = x:msg s}

tellFailing :: String -> Convert ()
tellFailing x = modify $ \s -> s {errs = x:errs s}

lookup' :: Eq a => a -> [(a,b)] -> [b]
lookup' a  =  map snd . filter ((== a) . fst)

head' :: String -> [a] ->  a
head' s []     = error $ "Error in head in "++s
head' _ (x:xs) = x
