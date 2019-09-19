{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Main extraction algorithm

module Saldoer (doExtract, extract) where

import System.IO (hClose, openTempFile, stdout, hSetBuffering, BufferMode(..))
import System.Process (rawSystem)
import System.Exit (ExitCode(..))
import System.Directory (removeFile, copyFile)
import System.FilePath ((</>),(<.>))

import Data.Char ({-isDigit,-}isAlpha)
import Data.Maybe (catMaybes,mapMaybe)
import qualified Data.Map.Strict as M
import Data.List (delete, sortOn)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf

import Control.Monad (when, unless, zipWithM)
import Control.Monad.Trans (lift)
import Control.Monad.State (StateT(..), modify, gets, execStateT)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Arrow (first)
import Control.Exception (onException)

import qualified PGF

import Common (Entry(..), Lex, Table, nl, writeReportFile)

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
  { stErrs :: [String]
  , stMsg :: [String]
  , stRetries :: [GrammarInfo]
  , stPGF :: FilePath
  , stOK :: [GrammarInfo]
  , stLex :: Lex
  , stChanges :: Int
  , stDead :: [Text]
  , stTmps :: [FilePath]
  , stPartNo :: Int
  , stSelection :: Maybe [Text]
  , stName :: String
}

data GrammarInfo = G
  { grLemma :: Text
  , grPOS :: Text
  , grForms :: [[Text]]
  , grExtra :: Text
  , grFunctions :: (Text,Text)
  , grParadigms :: Paradigm
  } deriving Show

type Paradigm = [(Text,[Text],Text)]

instance Eq GrammarInfo where
  -- checks equality on the name only, in order to simplify deletion from retries list
  g1 == g2 = grLemma g1 == grLemma g2

-----------------------------------------------------------------------------

-- | Main entry point: extract all parts and write final GF modules
doExtract :: [Lex] -> Int -> IO ()
doExtract lexs skip = do
  let
    name = "Tot"
    run lexi n = do
      printf "\nExtracting part %d of %d\n" (n+1) (length lexs)
      extract Nothing name lexi n
  entriess <- zipWithM run lexs [skip..]
  let entriesSorted = sortOn grLemma (concat entriess) :: [GrammarInfo]
  nl

  -- Find entries with only one sense and create numberless lins for them
  let
    gf_ids :: Set Text -- idents
    gf_ids = S.fromList $ map (T.pack . mkGFName) entriesSorted

    uniqs :: [GrammarInfo]
    uniqs = flip filter entriesSorted $ \g ->
      let
        lemgram = grLemma g -- kille..nn.1
        -- gfid1 = mkGFName g -- kille_1_N
        gfid2 = T.pack $ mkGFName (g {grLemma = T.dropEnd 1 lemgram `T.snoc` '2'}) -- kille_2_N
      in T.last lemgram == '1' && S.notMember gfid2 gf_ids

    shadows :: [(String,String)] -- abs, cnc
    shadows =
      [ (abs,cnc)
      | g <- uniqs
      , let oldId = mkGFName g -- vala_1_N
      , let newId = T.pack $ mkGFNameNumberless g -- vala_N
      , newId `S.notMember` gf_ids
      , let cat = grPOS g
      , let abs = printf "  %s : %s ; -- %s\n" newId cat oldId
      , let cnc = printf "  %s = %s ;\n" newId oldId
      ]

  printf "Adding numberless functions for %d of %d lemmas\n\n" (length shadows) (S.size gf_ids)

  writeReportFile (genDir </> "DictSweAbs.gf") $ concat
    [ absHeader "DictSweAbs"
    , concatMap showAbs entriesSorted
    , "\n  -- Numberless\n"
    , concatMap fst shadows
    , "}\n"
    ]
  writeReportFile (genDir </> "DictSwe.gf") $ concat
    [ concHeader "DictSwe" "DictSweAbs"
    , concatMap showCnc entriesSorted
    , "\n  -- Numberless\n"
    , concatMap snd shadows
    , "}\n"
    ]

-- | Extract individual part
extract :: Maybe [Text] -> String -> Lex -> Int -> IO [GrammarInfo]
extract select name saldo n  = do
  hSetBuffering stdout NoBuffering
  let
    run = do
      createGF saldo
      compileGF
      loop
      printGFFinal
      cleanUp
  mst <- runExceptT $ execStateT run (initState n select name)
  (entries,errs) <- case mst of
    Left er  -> return ([],er)
    Right st -> do
      let ms = unlines (reverse (stMsg st))
      let fails = unlines $ "Retries:" : map show (stRetries st) ++ "Dead:" : map T.unpack (stDead st)
      writeFile (logFile "messages" n) ms
      writeFile (logFile "fail" n) fails
      -- appendCode name $ stOK st
      return (stOK st, unlines (reverse (stErrs st)))
  writeFile (logFile "errors" n) errs
  return entries

  where
    logFile :: String -> Int -> FilePath
    logFile s n = logDir </> s++show n <.> "txt"

    loop = do
      updateGF
      compileGF
      changes <- gets stChanges
      unless (changes == 0) loop

-------------------------------------------------------------------
-- Bootstrap initial version of GF
-------------------------------------------------------------------

createGF :: Lex -> Convert ()
createGF sal = do
  io $ putStr "Creating GF source"
  mapM_ findGrammar (M.toList sal)
  todo <- gets stRetries
  when (null todo) $ fail "No words from this section. Skipping"
  modify $ \s -> s {stLex = sal}
  printGF
  report "Lexicon created"
  io nl

findGrammar :: (Text,Entry) -> Convert ()
findGrammar (id,E pos table) =  do
  rest <- gets stSelection
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
      modify $ \s -> s {stRetries = cnc++stRetries s}

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
  tmp_saldo <- gets stPGF
  io $ putStrLn "Reading PGF"
  pgf <- io $ PGF.readPGF tmp_saldo
  io $ putStrLn "Updating GF source"
  cnc_file <- gets stRetries
  modify $ \s -> s {stChanges = 0, stRetries = []}
  report $ "updating "++ unlines (map show cnc_file)
  mapM_ (check pgf) cnc_file
  printGF
  c <- gets stChanges
  let update = printf "Updated %d words" c
  io $ putStrLn update
  report update

check :: PGF.PGF -> GrammarInfo -> Convert ()
check gf entry@(G id cat lemmas _ _ _) = do
  saldo <- gets stLex
  case M.lookup id saldo of
       Just (E p t) -> checkWord gf t entry
       Nothing      -> do tellFailing (printf "unknown id in SALDO: %s" id)
                          isDead id

packTuple :: (String,String) -> (Text,Text)
packTuple (a,b) = (T.pack a, T.pack b)

checkWord :: PGF.PGF -> Table -> GrammarInfo -> Convert ()
checkWord gf t entry@(G id cat lemmas _ _ _) = do
  langName <- getLangName
  let
    gf_t = map packTuple $ concat $ PGF.tabularLinearizes gf (read langName) (read (mkGFName entry))
    paramMap = head' "check" [map | (_,gf_cat,map,_,_) <- catMap, gf_cat == cat]
  checkForms paramMap t gf_t entry

checkForms :: [(Text, [Text])] -> Table -> Table -> GrammarInfo -> Convert ()
checkForms paramMap fm_t gf_t entry@(G id cat lemmas _ _  _)
  | null diffs = accept entry
  | otherwise  = do
      c <- gets stChanges
      modify $ \s -> s {stChanges = c+1}
      report $ printf "redo word %s" id
      report (show [[(lookup f fm_t,lookup g' gf_t) | g' <- g ] | (f,g) <- paramMap])
      getNextLemma $! entry
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

    getNextLemma (G id cat lemmas _ _ []) = do
        tellFailing (printf "no more paradigms to choose from: %s" id)
        isDead id
    getNextLemma (G id cat lemmas b f ((pre,xs,a):ps)) = do
        report $ printf "working on %s" id
        report $ printf "next paradigm: %s" (show xs)
        report $ printf "to choose from: %s" (show ps)
        forms <- mapM getLemma xs
        if Nothing `elem` forms
        -- to do: if the comparative forms for an adjective doesn't exist, add compounda
        then do
          report (printf "all forms for %s not found" id)
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
  io $ putStrLn "Compiling GF"
  concName <- getCncName
  pgfName  <- getPGFName
  res <- io $ rawSystem "gf" ["--quiet", "--batch", "--make", "--output-dir", buildDir, concName, "+RTS", "-K64M"]
  case res of
    ExitSuccess      -> return ()
    ExitFailure code -> do
      n <- gets stPartNo
      io $ putStrLn "Failed to create PGF.\nWill skip this section"
      fail (printf "Compiliation failed with error code %d\nPartition %d will be skipped" code n)
  (fpath,h) <- io $ openTempFile buildDir "tmp.pgf"
  io $ hClose h
  io $ copyFile pgfName fpath
  addTemp fpath
  report "compilation done"
  setPGF fpath

-- | Generate GF identifier from GrammarInfo term
mkGFName :: GrammarInfo -> String
mkGFName G{grLemma = id', grPOS = cat} =
  printf "%s_%c_%s" name num (toGFcat cat)
  where
    -- pfxnum x = if isDigit (T.head x) then 'x' `T.cons` x else x -- don't start with a digit
    name = {-pfxnum $-} dash2us $ T.takeWhile (/= '.') id'
    num = T.last id'

-- | Generate GF identifier from GrammarInfo term, omitting sense number
mkGFNameNumberless :: GrammarInfo -> String
mkGFNameNumberless G{grLemma = id', grPOS = cat} =
  printf "%s_%s" name (toGFcat cat)
  where
    name = dash2us $ T.takeWhile (/= '.') id'

dash2us :: Text ->  Text
dash2us = T.replace "-" "_"

toGFcat :: Text ->  Text
toGFcat "VR" = "V"
toGFcat "VP" = "V"
toGFcat  v   = v

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
  good <- gets stOK
  num <- gets stPartNo
  io $ printGF' good (show num) "Fin"

printGF :: Convert ()
printGF = do
  new  <- gets stRetries
  good <- gets stOK
  let entries = new++good
  num <- gets stPartNo
  nam <- gets stName
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
showAbs entry@(G id cat _ _ _ _) =
  printf "  %s : %s ; -- %s\n" (mkGFName entry) (convert cat) id
  where
    convert "VR" = "V"
    convert "VP" = "V"
    convert x    = x

showCnc :: GrammarInfo -> String
showCnc entry@(G id _ [[]] _ _ _) = printf "-- %s does not have enough forms\n" (mkGFName entry)
showCnc entry@(G id _ lemmas a (mk,end) _) =
  printf "  %s = %s %s%s%s ;\n" (mkGFName entry) mk defs extra end
  where
    defs = T.unwords [ T.unwords (map fnutta lemma_v) | lemma_v <- lemmas]
    -- defs = T.unwords [ if null lemma_v then "(variants {})" else T.unwords (map fnutta lemma_v) | lemma_v <- lemmas]
    extra = if T.null a then "" else T.concat [" ", a, " "]
    fnutta :: Text -> Text
    -- fnutta x@"variant {}" = T.concat ["(", x, ")"]
    fnutta x = T.concat ["\"", x, "\""]

absHeader :: String -> String
absHeader name = unlines
  [ "--# -path=.:abstract:alltenses/"
  , printf "abstract %s = Cat ** {" name
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
 n   <- gets stPartNo
 nam <- gets stName
 return $ genDir </> "saldo"++nam++show n++"Cnc" <.> "gf"

-- getAbsName :: Convert String
-- getAbsName = do
--  n   <- gets stPartNo
--  nam <- gets stName
--  return $ genDir </> "saldo"++nam++show n <.> "gf"

getLangName :: Convert String
getLangName = do
 n   <- gets stPartNo
 nam <- gets stName
 return $ "saldo"++nam++show n++"Cnc"

getPGFName :: Convert String
getPGFName = do
 n   <- gets stPartNo
 nam <- gets stName
 return $ buildDir </> "saldo"++nam++show n <.> "pgf"

cleanUp :: Convert ()
cleanUp = do
  ts <- gets stTmps
  io $ mapM_ (\t -> onException (removeFile t) (return ())) ts

-------------------------------------------------------------------
-- State
-------------------------------------------------------------------

initState :: Int -> Maybe [Text] -> String -> CState
initState n s name = CS
  { stErrs = []
  , stMsg = []
  , stRetries = []
  , stPGF = ""
  , stOK = []
  , stDead = []
  , stLex = M.empty
  , stChanges = 0
  , stTmps = []
  , stPartNo = n
  , stSelection = s
  , stName = name
  }

setPGF :: FilePath -> Convert ()
setPGF f = modify $ \s -> s { stPGF = f}

replace :: GrammarInfo -> Convert ()
replace gr = modify $ \s -> s {stRetries = gr : delete gr (stRetries s)} -- hehe, make nice?

-- add to dead , remove from retries
isDead :: Text -> Convert ()
isDead d = modify $ \s -> s {stDead = d:stDead s, stRetries = delete (emptyG d) (stRetries s)}  -- hehe, make nice?
  where emptyG d = G d "" [] "" ("","") []

--add to ok, remove from retries
accept :: GrammarInfo -> Convert ()
accept e = do
  report $ printf "accepting %s" (grLemma e)
  modify $ \s -> s {stRetries = delete e (stRetries s), stOK = e:stOK s}
  r <- gets stRetries
  report $ printf "deleted %s from retries. result: %s" (grLemma e) (show (e `elem` r))

-- setPartNo :: Int -> Convert ()
-- setPartNo n = modify $ \s -> s {stPartNo = n}

addTemp :: FilePath -> Convert ()
addTemp f = modify $ \s -> s {stTmps = f:stTmps s}

-------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------

io :: IO a -> Convert a
io = lift . lift

report :: String -> Convert ()
report x = modify $ \s  -> s {stMsg = x:stMsg s}

tellFailing :: String -> Convert ()
tellFailing x = modify $ \s -> s {stErrs = x:stErrs s}

-- | Find all values matching key in non-unique key-value list
lookup' :: Eq a => a -> [(a,b)] -> [b]
lookup' a  =  map snd . filter ((== a) . fst)

-- | Head which reports context on failure
head' :: String -> [a] ->  a
head' s []     = error $ "Error in head in "++s
head' _ (x:xs) = x
