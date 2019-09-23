{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Main extraction algorithm

module Saldoer (doExtract, extract) where

import Prelude hiding (show, print) -- use ushow, uprint instead

import System.IO (hClose, openTempFile, stdout, hSetBuffering, BufferMode(..))
import System.Process (rawSystem)
import System.Exit (ExitCode(..))
import System.Directory (removeFile, copyFile)
import System.FilePath ((</>), (<.>))

import Data.Char (isAlpha)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf
import Text.Show.Unicode (uprint, ushow)

import Control.Monad (when, unless, zipWithM)
import Control.Monad.Trans (lift)
import Control.Monad.State (StateT(..), modify, gets, execStateT)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Exception (onException)

import qualified PGF

import Common
import Paradigms

show :: Show a => a -> String
show = ushow

print :: Show a => a -> IO ()
print = uprint

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
  { grLemma :: Text -- ^ SALDO lemgram, e.g. "blod..nn.1"
  , grPOS :: Text -- ^ GF category
  , grForms :: [Text] -- ^ string arguments for paradigm
  , grExtra :: Text -- ^ other arguments, e.g. gender
  , grFunction :: (Text,Text) -- ^ function name (e.g. "mkV"), particle (e.g. [åka] "hem")
  , grParadigms :: ParadigmList -- ^ queue of paradigms to try
  } deriving Show

showG :: GrammarInfo -> String
-- showG = T.unpack . T.replace "{gr" "{\n  gr" . T.replace ", gr" "\n  gr" . T.pack . show
showG g = L.intercalate "\n"
  [ "G {"
  , printf "  grLemma = %s" (show (grLemma g))
  , printf "  grPOS = %s" (show (grPOS g))
  , printf "  grForms = %s" (show (grForms g))
  , printf "  grExtra = %s" (show (grExtra g))
  , printf "  grFunction = %s" (show (grFunction g))
  , printf "  grParadigms =\n%s" (showListIndent 4 (grParadigms g))
  , "}"
  ]

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
  let entriesSorted = L.sortOn grLemma (concat entriess) :: [GrammarInfo]
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
    , if null shadows then "" else "\n  -- Numberless\n"
    , concatMap fst shadows
    , "}\n"
    ]
  writeReportFile (genDir </> "DictSwe.gf") $ concat
    [ concHeader "DictSwe" "DictSweAbs"
    , concatMap showCnc entriesSorted
    , if null shadows then "" else "\n  -- Numberless\n"
    , concatMap snd shadows
    , "}\n"
    ]

-- | Extract individual part
extract :: Maybe [Text] -> String -> Lex -> Int -> IO [GrammarInfo]
extract select name saldo n  = do
  hSetBuffering stdout NoBuffering
  let
    loop = do
      report ""
      updateGF
      compileGF
      changes <- gets stChanges
      unless (changes == 0) loop
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

-----------------------------------------------------------------------------
-- Bootstrap initial version of GF

createGF :: Lex -> Convert ()
createGF sal = do
  io $ putStr "Creating GF source"
  mapM_ findGrammar (M.toList sal)
  todo <- gets stRetries
  when (null todo) $ fail "No words from this section. Skipping"
  modify $ \s -> s {stLex = sal}
  printGF
  report "lexicon created"
  io nl

-- TODO split entries with variants into multiple GrammarInfo terms
-- We don't want to use GF variants to encode them
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
            [ G id gf_cat forms extra funcs paradigms
            | (gf_cat,(f,f'),paradigms) <- xs
            , let forms = [snd $ head' "createGF" table]
            , let extra = ""
            , let funcs = (f,findA gf_cat (T.append id f'))
            ]
      modify $ \s -> s {stRetries = stRetries s ++ cnc}

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

-----------------------------------------------------------------------------
-- Compare the current GF version with SALDO

-- | Check all items in retries list
updateGF :: Convert ()
updateGF = do
  tmp_saldo <- gets stPGF
  io $ putStrLn "Reading PGF"
  pgf <- io $ PGF.readPGF tmp_saldo
  io $ putStrLn "Updating GF source"
  gis <- gets stRetries
  modify $ \s -> s {stChanges = 0, stRetries = []}
  report $ "will update:\n" ++ L.intercalate "\n" (map showG gis)
  mapM_ (check pgf) gis
  printGF
  c <- gets stChanges
  let update = printf "Updated %d words" c
  io $ putStrLn update
  report update

-- | Check individual item
-- 1: get table from SALDO
check :: PGF.PGF -> GrammarInfo -> Convert ()
check pgf entry@(G id _ _ _ _ _) = do
  saldo <- gets stLex
  case M.lookup id saldo of
    Just (E p t) -> checkWord pgf t entry
    Nothing -> do
      tellFailing (printf "unknown id in SALDO: %s" id)
      isDead id

-- 2: get table from PGF
checkWord :: PGF.PGF -> Table -> GrammarInfo -> Convert ()
checkWord pgf t entry@(G _ cat _ _ _ _) = do
  langName <- getLangName
  let
    packTuple (a,b) = (T.pack a, T.pack b)
    gf_t = map packTuple $ concat $ PGF.tabularLinearizes pgf (read langName) (read (mkGFName entry))
    paramMap = head' "checkWord" [map | (_,gf_cat,map,_,_) <- catMap, gf_cat == cat] -- find relevant paramMap for cat
  checkForms paramMap t gf_t entry

-- 3: compare the two tables
checkForms :: ParamMap -> Table -> Table -> GrammarInfo -> Convert ()
checkForms paramMap fm_t gf_t entry@(G id _ _ _ (mk,_) _) = do
  report $ "fm_t:\n" ++ showListIndent 2 fm_t
  report $ "gf_t:\n" ++ showListIndent 2 gf_t
  report $ "comp:\n" ++ showListIndent 2 comp
  case comp of
    _ | not anyDiffs -> accept entry
    _ | anyMissing && mk == "mkN" && and
        [ sg_fm_vs == d
        | (a,b,_,d) <- comp
        , null b -- form missing from SALDO
        , "pl " `T.isPrefixOf` a  -- "pl indef nom"
        , let sg = T.replace "pl " "sg " a -- "sg indef nom"
        , let sg_fm_vs = lookup' sg fm_t :: [Text]
        ] -> accept entry
    _ -> do
      c <- gets stChanges
      modify $ \s -> s {stChanges = c+1}
      report $ printf "redo word %s" id
      getNextLemma $! entry
  where
    -- comparison of SALDO / GF tables
    comp :: [(Text,[Text],[Text],[Text])]
    comp =
      [ (fm_p,fm_vs,gf_p,gf_vs)
      | (fm_p,gf_p) <- paramMap -- fm_p :: Text, gf_p :: [Text]
      , let fm_vs = lookup' fm_p fm_t -- fm_vs :: [Text]
      , let gf_vs = concatMap (`lookup'` gf_t) gf_p -- gf_vs :: [Text]
      , let b = fm_vs /= gf_vs
      ]

    anyDiffs :: Bool
    anyDiffs = any (\(_,fm_vs,_,gf_vs) -> fm_vs /= gf_vs) comp

    anyMissing :: Bool
    anyMissing = any (\(_,fm_vs,_,_) -> null fm_vs) comp

    getNextLemma :: GrammarInfo -> Convert ()
    getNextLemma (G id cat _ _ _ []) = do
      let msg = printf "no more paradigms to choose from: %s" id
      report msg
      tellFailing msg
      isDead id
    getNextLemma (G id cat lemmas b f (p@(pre,xs,a):ps)) = do
      report $ printf "working on %s" id
      report $ printf "next paradigm:\n  %s" (show p)
      report $ "remaining paradigms:\n" ++ showListIndent 2 ps
      forms <- mapM getLemma xs
      report $ printf "forms: %s" (show forms)
      if Nothing `elem` forms
      -- TODO if the comparative forms for an adjective doesn't exist, add compounda
      then do
        report (printf "all forms for %s not found" id)
        getNextLemma (G id cat lemmas b f ps)
      else replace (G id cat (catMaybes forms) a f ps)
      where
        getLemma :: Text -> Convert (Maybe Text)
        getLemma gf_p =
          case lookup fm_p fm_t of
            Just "" -> do
              report (printf "form '%s' is empty for %s" gf_p id)
              return Nothing
            Just fm_v ->
              return $ Just fm_v
            x -> do
              report (printf "form '%s' does not exist in %s" gf_p id)
              return Nothing
          where
            fm_ps :: [Text]
            fm_ps = [fm_p | (fm_p,gf_p') <- paramMap, gf_p `elem` gf_p']

            fm_p :: Text
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

-----------------------------------------------------------------------------
-- Dump GF code

-- | Generate GF identifier from GrammarInfo term
mkGFName :: GrammarInfo -> String
mkGFName G{grLemma = id', grPOS = cat} =
  printf "%s_%s_%s" name num (toGFcat cat)
  where
    name = dash2us $ T.takeWhile (/= '.') id'
    num = T.takeWhileEnd (/='.') id'

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
showCnc entry@(G id _ [] _ _ _) = printf "-- %s does not have enough forms\n" (mkGFName entry)
showCnc entry@(G id _ forms a (mk,end) _) =
  printf "  %s = %s %s%s%s ;\n" (mkGFName entry) mk defs extra end
  where
    defs = T.unwords $ map fnutta forms
    extra = if T.null a then "" else T.concat [" ", a]
    fnutta :: Text -> Text
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

-----------------------------------------------------------------------------
-- State

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

-- | Replace element in retries
-- Works because of defininition of equality of GrammarInfo
replace :: GrammarInfo -> Convert ()
replace gr = modify $ \s -> s {stRetries = replaceInList gr gr (stRetries s)}

-- | Replace element in list. Will append to end if not in original list.
replaceInList :: Eq a => a -> a -> [a] -> [a]
replaceInList x y xs = a ++ y : drop 1 b
  where (a,b) = L.break (x ==) xs

-- add to dead , remove from retries
isDead :: Text -> Convert ()
isDead d = modify $ \s -> s {stDead = d:stDead s, stRetries = L.delete (emptyG d) (stRetries s)}  -- hehe, make nice?
  where emptyG d = G d "" [] "" ("","") []

--add to ok, remove from retries
accept :: GrammarInfo -> Convert ()
accept e = do
  report $ printf "accepting %s" (grLemma e)
  modify $ \s -> s {stRetries = L.delete e (stRetries s), stOK = e:stOK s}
  r <- gets stRetries
  report $ printf "deleted %s from retries. result: %s" (grLemma e) (show (e `elem` r))

-- setPartNo :: Int -> Convert ()
-- setPartNo n = modify $ \s -> s {stPartNo = n}

addTemp :: FilePath -> Convert ()
addTemp f = modify $ \s -> s {stTmps = f:stTmps s}

-----------------------------------------------------------------------------
-- Helpers

io :: IO a -> Convert a
io = lift . lift

report :: String -> Convert ()
report x = modify $ \s  -> s {stMsg = x:stMsg s}

tellFailing :: String -> Convert ()
tellFailing x = modify $ \s -> s {stErrs = x:stErrs s}

-- | Find all values matching key in non-unique key-value list
-- lookup' a [(a,1),(b,2),(a,3)] == [1,3]
lookup' :: Eq a => a -> [(a,b)] -> [b]
lookup' a  =  map snd . filter ((== a) . fst)

-- | Head which reports context on failure
head' :: String -> [a] ->  a
head' s []     = error $ "Error in head in "++s
head' _ (x:xs) = x

showListIndent :: Show a => Int -> [a] -> String
showListIndent n x = tab ++ L.intercalate ("\n" ++ tab) (map show x)
  where tab = L.replicate n ' '
