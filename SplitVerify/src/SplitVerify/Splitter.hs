{-# OPTIONS_GHC -Wall -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wpartial-fields -Wredundant-constraints #-}
{-# LANGUAGE DefaultSignatures #-}
module SplitVerify.Splitter (chunk) where
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (splitOn)
import qualified RIO.List as L
import RIO
import Text.Boomerang.String (parseString, unparseString, StringError)
import SplitVerify.Settings
import SplitVerify.JavaStructures
import SplitVerify.JavaParse (javaFile)
import SplitVerify.GenericClasses
import qualified RIO.Set as Set
import qualified RIO.Map as Map
import qualified RIO.Char as C
-- import qualified Data.Map.Merge.Strict as Map (mergeA,WhenMissing)

-- This file contains the pure (non-IO) functions that parse a file into separate proof-obligations.
-- The main function is 'chunk'

chunk :: Criterion -> [T.Text] -> Either StringError [ ( [T.Text] , [T.Text] ) ]
chunk _crit txts
  = case traverse (parseString javaFile . T.unpack) txts of
      (Right vs) -> let findDependents k = Map.findWithDefault (error "Map missing key") k dependents
                        dependents = fixMap (getDependencies vs)
                    in Right [ L.unzip
                               [ ( partialUnparse v, partialUnparse (normalise $ removeWS v) )
                               | v <- vs' ]
                             | vs' <- map (silence (Map.keysSet dependents)) vs : -- silence everything to verify Mathematicals. TODO: when Mathematicals can be silenced, remove.
                                      Set.elems (Set.map (normalise . snd) $ split findDependents vs) ]
      (Left e) -> Left e
  where fromMaybeFail _ (Just s) = T.pack s
        fromMaybeFail x Nothing = error ("Failed to print "++show x)
        partialUnparse x = fromMaybeFail x (unparseString javaFile x)
        fixMap mp = case Map.mapAccumWithKey fixMapAcc (False,mp) mp of
                      ((True,_),mp') -> fixMap mp'
                      ((False,_),mp') -> mp'
        fixMapAcc (upd,lkpmp :: Map T.Text (Set T.Text)) from_itm to_set
          = let find_itm to_itm = Map.findWithDefault (Set.singleton to_itm) to_itm lkpmp
                new_set = Set.union to_set . Set.unions . Set.map find_itm $ to_set
                new_lkpmp = Map.insert from_itm new_set lkpmp
            in ((upd || new_set /= to_set,new_lkpmp),new_set)

-------------------------
-- Overview of classes --
-------------------------

-- Splittable and silence:
-- Splittable class: uses Silence class to mute the non-split parts
-- gets an argument to determine the dependencies (which silence uses)

-- Dependencies class: uses getIds to get the identifiers which are used
-- Ids class is for getIds
-- also uses the function extractName to get the defined thing from method headers

-- After silencing, some parts of the data structure are empty that shouldn't be
-- Normalise gets rid of those.

class (Ord a) => Splittable a where
  -- | split takes a verifiable thing and splits it into separate proof obligations
  split :: (Text -> Set.Set Text) -- ^ given the ID of a function, give everything that depends on it
        -> a -- ^ the thing that is to be split
        -> Set.Set (Text, a) -- ^ ways the thing can be split (other parts are silenced)
-- | Remove everything not in the set, leave only the contracts for what is in the set
class Silence a where silence :: Set.Set Text -> a -> a


instance Splittable ClassItem where
  -- TODO: find a way to silence Mathematical too. Requires a VerCors adjustment
  split _ o@(Method _ hdr (Sequential _)) = Set.singleton (extractName hdr, o)
  split _ _ = Set.empty
instance (Splittable b,Silence b) => Splittable (SepBy TextWithComments b) where
  split _ (Sepd _ Nothing) = Set.empty -- separator does not contain verifiable things
  split s (Sepd a (Just (b,rs)))
    = Set.union 
      (Set.map (\(idT, v) -> (idT,Sepd a (Just (v, silence (s idT) rs)))) $ split s b)
      (Set.map (\(idT, v) -> (idT,Sepd a (Just (silence (s idT) b, v )))) $ split s rs)
instance Splittable ClassDescription where
  split s (ClassDescription h c) = Set.map (fmap $ ClassDescription h) (split s c)
instance (Splittable a, Silence a) => Splittable [a] where
  split s xs
   = Set.unions [ Set.map (\(idT,v) -> (idT, map (silence (s idT)) pre ++ v:map (silence (s idT)) suf)) sx
                -- list xs is split in three: (prefix, (split element x, suffix))
                | (pre, (sx, suf)) <- zip (L.inits xs) (zip (map (split s) xs) (drop 1 (L.tails xs)))]

instance Silence ClassItem where
  silence _ (Declaration x) = Declaration x
  silence s o@(Method contr hdr defn) | Set.member (extractName hdr) s
    = case defn of
        Sequential t -> Method contr hdr (AssumeFalse (countNewlines t))
        _ -> o
    | otherwise = NoItem . fromString . take (countNewlines o) $ L.repeat '\n'
  silence _ (NoItem x) = NoItem x
instance Silence ClassDescription where
  silence s (ClassDescription t c) = ClassDescription t (silence s c)
instance Silence b => Silence (SepBy TextWithComments b) where
  silence _ o@(Sepd _ Nothing) = o
  silence s (Sepd a (Just (b, rs))) = Sepd a (Just (silence s b, silence s rs))

class Dependencies a where getDependencies :: a -> Map.Map Text (Set.Set Text)
class Ids a where getIds :: a -> Set.Set Text

instance Dependencies [JavaFile] where
  getDependencies jvfs = Map.unionsWith Set.union (map getDependencies jvfs)
instance Dependencies b => Dependencies (SepBy TextWithComments b) where
  getDependencies (Sepd _ mb)
    = case mb of Nothing -> Map.empty
                 Just (b, rs) -> Map.unionWith Set.union (getDependencies b) (getDependencies rs)
instance Dependencies ClassDescription where
  getDependencies (ClassDescription _ b) = getDependencies b
instance Dependencies ClassItem where
  getDependencies (Method (Contract ctr) hdr df)
   = Map.singleton (extractName hdr) (Set.union (getIds ctr) (getIds df))
  getDependencies _ = Map.empty
  
instance Ids a => Ids [a] where
  getIds = Set.unions . map getIds
instance Ids DefinitionExpr where
  getIds (Sequential content) = getIds content
  getIds (Mathematical content) = getIds content
  getIds _ = Set.empty
instance Ids Condition where
  getIds (Condition _kw content _spaces) = getIds content
instance Ids BalancedPart where
  getIds (BracketedRound x) = getIds x
  getIds (BracketedStach x) = getIds x
  getIds (NoBrackets (TextNoComment x)) = getIds x
  getIds (NoBrackets _) = Set.empty
instance Ids Text where
  getIds txt = Set.fromList (breakApart txt)
 
data Normalise_
class Normalise a where
  -- remove empty comments, declarations etc, to make things printable again
  normalise :: a -> a
  default normalise :: MapGeneric Normalise_ a => a -> a
  normalise = mapGeneric (Phantom::Phantom Normalise_)
instance Normalise a => FmapInstance Normalise_ a where
  gmapinstance _ = normalise

instance Normalise JavaFile where
instance Normalise [JavaFile] where
instance Normalise [TextOrComment] where
  normalise (t:ts)
   = (case t of
       (TextNoComment txt) | T.null txt -> id
       o -> (:) o) (normalise ts)
  normalise [] = []
instance Normalise (Maybe (ClassDescription, JavaFile)) where
instance Normalise (ClassDescription, JavaFile) where
  normalise (a, b) = (normalise a, normalise b)
instance Normalise ClassDescription where
instance Normalise (SepBy TextWithComments ClassItem) where
  normalise (Sepd toc ci) = case ci of
    Nothing -> Sepd (normalise toc) Nothing
    Just (NoItem txt, Sepd toc2 ci2) -> normalise (Sepd (toc ++ (TextNoComment txt:toc2)) ci2)
    Just (x, r) -> Sepd (normalise toc) (Just (normalise x, normalise r))
instance Normalise ClassItem where
  normalise (Method c h d) = Method c (normalise h) (normalise d)
  normalise (Declaration b) = Declaration (normalise b)
  normalise (NoItem txt) = NoItem txt
instance Normalise MethodHeader where
instance Normalise DefinitionExpr where
instance Normalise Int where -- to prevent writing an instance for DefinitionExpr
  normalise = id
instance Normalise [BalancedPart] where
  normalise (x:rs) = case x of
                       (NoBrackets (TextNoComment txt)) | T.null txt -> normalise rs
                       o -> o : normalise rs
  normalise [] = []

extractName :: MethodHeader -> Text
extractName (MethodHeader nm _args _spcs)
 = case concat . map breakApartTOC $ nm of
    [] -> error "Method has no name (due to parse error?)"
    (h:rs) -> L.foldl (flip const) h rs -- == last (h:rs)

breakApartTOC :: TextOrComment -> [Text]
breakApartTOC (TextNoComment x) = breakApart x
breakApartTOC _ = []

breakApart :: Text -> [Text]
breakApart
 = wrd_to_lst . T.foldr ba ([],"")
 where
  ba chr (lst,wrd) -- we work in String until we get a complete word, for a fast prepend of Char (at the cost of some memory overhead the size of one word)
    = case not (C.isAscii chr) || C.isAlphaNum chr || chr == '_' of
        True -> (lst,chr:wrd)
        False -> (wrd_to_lst (lst, wrd), [])
  wrd_to_lst (lst,[]) = lst
  wrd_to_lst (lst,w) = fromString w:lst
  

data HasRefs_
class HasRefs a where
  getRefs :: a -> Set.Set Text
  default getRefs :: FoldGeneric HasRefs_ (Set.Set Text) a => a -> Set.Set Text
  getRefs = foldGeneric (Phantom :: Phantom HasRefs_)
instance HasRefs a => GFoldInstance HasRefs_ (Set.Set Text) a where
  gfoldInstance _ = getRefs
instance Ord a => (GFoldOp HasRefs_ (Set.Set a)) where
  gop _ = Set.union
  gzero _ = Set.empty

instance HasRefs Text where
  getRefs = Set.fromList . T.split (\x -> x <= ' ')


data HasNewlines_
class HasNewlines a where
  countNewlines :: a -> Int
  default countNewlines :: FoldGeneric HasNewlines_ Int a => a -> Int
  countNewlines = foldGeneric (Phantom :: Phantom HasNewlines_)
instance HasNewlines a => GFoldInstance HasNewlines_ Int a where
  gfoldInstance _ = countNewlines
instance (GFoldOp HasNewlines_ Int) where
  gop _ = (+)
  gzero _ = 0

instance HasNewlines T.Text where
  countNewlines t = length (T.splitOn "\n" t) - 1
instance HasNewlines TextOrComment where
instance HasNewlines BalancedPart where
instance HasNewlines ClassItem where
instance HasNewlines Contract where
instance HasNewlines Condition where
instance HasNewlines MethodHeader where
instance HasNewlines CondKeyword where
instance HasNewlines a => HasNewlines [a] where
instance HasNewlines DefinitionExpr where
  countNewlines NoDefinition = 0
  countNewlines (Sequential b)   = countNewlines b
  countNewlines (Mathematical b) = countNewlines b
  countNewlines (AssumeFalse i)  = i

data WhiteSpaced_
class WhiteSpaced a where
  removeWS :: a -> a -- remove as much whitespace as possible without changing semantics
  default removeWS :: (MapGeneric WhiteSpaced_ a) => a -> a
  removeWS = mapGeneric (Phantom::Phantom WhiteSpaced_)
instance WhiteSpaced a => FmapInstance WhiteSpaced_ a where
  gmapinstance _ = removeWS

instance WhiteSpaced Condition where
instance WhiteSpaced CondKeyword where
instance WhiteSpaced Contract where
instance WhiteSpaced a => WhiteSpaced (Maybe a) where
instance WhiteSpaced [TextOrComment] where
  removeWS = map TextNoComment . L.filter (not . T.null) . map (removeWS . getNls)
    where getNls (TextCommentLine _ x) = x
          getNls (TextCommentInLine x) = T.filter (== '\n') x
          getNls (TextNoComment t) = t
instance WhiteSpaced Text where
  removeWS
   = T.intercalate " "
     . L.filter (not . T.null)
     . concatMap (T.splitOn "\r")
     . concatMap (T.splitOn "\n")
     . T.splitOn " "
instance WhiteSpaced Balanced where -- list of BalancedPart
  removeWS (BracketedRound x:xs)
    = BracketedRound (removeWS x) : removeWS xs
  removeWS (BracketedStach x:xs)
    = BracketedStach (removeWS x) : removeWS xs
  removeWS [] = []
  removeWS xs
    = (map NoBrackets . removeWS $ init_part) ++ removeWS post_part
    where (init_part,post_part) = getIP [] xs
          getIP res (NoBrackets y:ys) = getIP (y : res) ys
          getIP res ys = (res,ys)

instance WhiteSpaced [Condition] where
instance (WhiteSpaced a, WhiteSpaced b) => WhiteSpaced (SepBy a b) where
instance (WhiteSpaced a, WhiteSpaced b) => WhiteSpaced (a, b) where
instance WhiteSpaced MethodHeader where
instance WhiteSpaced DefinitionExpr where
  removeWS NoDefinition = NoDefinition
  removeWS (Sequential b) = Sequential (removeWS b)
  removeWS (Mathematical b) = Mathematical (removeWS b)
  removeWS (AssumeFalse _i) = (AssumeFalse 0)
instance WhiteSpaced ClassItem where
instance WhiteSpaced ClassDescription where
