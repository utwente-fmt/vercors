{-# OPTIONS_GHC -Wall -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wpartial-fields -Wredundant-constraints #-}
{-# LANGUAGE DefaultSignatures #-}
module SplitVerify.Splitter (chunk) where
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (splitOn)
import qualified RIO.List as L (unzip)
import RIO
import Text.Boomerang.String (parseString, unparseString, StringError)
import SplitVerify.Settings
import SplitVerify.JavaStructures
import SplitVerify.JavaParse (javaFile)
import SplitVerify.GenericClasses
import qualified RIO.Set as Set

-- This file contains the pure (non-IO) functions that parse a file into separate proof-obligations.
-- The main function is 'chunk'

chunk :: Criterion -> [T.Text] -> Either StringError [ ( [T.Text] , [T.Text] ) ]
chunk _crit txts
  = case traverse (parseString javaFile . T.unpack) txts of
      (Right vs) -> Right [ L.unzip
                            [ ( partialUnparse v, partialUnparse (removeWS v) )
                            | v <- vs' ]
                          | vs' <- Set.elems (split vs)]
      (Left e) -> Left e
  where fromMaybeFail (Just s) = T.pack s
        fromMaybeFail Nothing = error ("Failed to print")
        partialUnparse = fromMaybeFail . unparseString javaFile 

-- by setting defaults, we can avoid having to write many instances
data Splittable_
class (Ord a) => Splittable a where
  split :: a -> Set.Set a -- ^ split into separate prove obligations
  silence :: a -> a -- ^ remove all prove obligations
  default silence :: (MapGeneric Splittable_ a) => a -> a
  silence = mapGeneric (Phantom::Phantom Splittable_)
instance Splittable a => FmapInstance Splittable_ a where
  gmapinstance _ = silence

instance Splittable a => Splittable (Maybe a) where
  split Nothing = Set.singleton Nothing
  split (Just a) = Set.map Just (split a)
instance (Splittable a, Splittable b) => Splittable (SepBy a b) where
  split (Sepd a b) = Set.union (Set.map (flip Sepd (silence b)) $ split a)
                               (Set.map (Sepd (silence a)) $ split b)
instance (Splittable a, Splittable b) => Splittable (a,b) where
  split (a,b) = Set.union (Set.map (flip (,) (silence b)) $ split a)
                          (Set.map ((,) (silence a)) $ split b)
instance Splittable ClassItem where
  split o
    = Set.singleton o
  silence o@(Method contr hdr defn)
    = case defn of
        Sequential t -> Method contr hdr (AssumeFalse (countNewlines t))
        _ -> o
  silence (Declaration x) = Declaration x
instance (Splittable a) => Splittable [a] where
  split (x:xs@(_:_))
   = Set.union (Set.map (:silence xs) (split x))
               (Set.map (silence x:) (split xs))
  split [x] = Set.map (:[]) (split x)
  split [] = Set.singleton []
instance Splittable TextOrComment where
  split x = Set.singleton x
  silence = id
instance Splittable ClassDescription where
  split (ClassDescription h c) = Set.map (ClassDescription h) (split c)

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
instance HasNewlines a => HasNewlines [a] where

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
instance WhiteSpaced a => WhiteSpaced [a] where
instance (WhiteSpaced a, WhiteSpaced b) => WhiteSpaced (SepBy a b) where
instance (WhiteSpaced a, WhiteSpaced b) => WhiteSpaced (a, b) where
instance WhiteSpaced TextOrComment where
  removeWS (TextCommentLine _ _) = (TextCommentLine "" "\n")
  removeWS (TextCommentInLine _) = TextCommentInLine ""
  removeWS (TextNoComment t) = TextNoComment (removeWS t)
instance WhiteSpaced BalancedPart where
instance WhiteSpaced MethodHeader where
instance WhiteSpaced DefinitionExpr where
  removeWS NoDefinition = NoDefinition
  removeWS (Sequential b) = Sequential (removeWS b)
  removeWS (Mathematical b) = Mathematical (removeWS b)
  removeWS (AssumeFalse _i) = (AssumeFalse 0)
instance WhiteSpaced ClassItem where
instance WhiteSpaced ClassDescription where
instance WhiteSpaced Text where
  removeWS
   = mergeSplit "\n" . replace "\r" "\n" . mergeSplit " "
   where mergeNulls (a:rst) = if T.null a then a:stripNulls rst else a:mergeNulls rst
         mergeNulls [] = []
         stripNulls [a] = [a]
         stripNulls (a:rst) = if T.null a then stripNulls rst else a:mergeNulls rst
         stripNulls [] = []
         mergeSplit s = T.intercalate s . mergeNulls . T.splitOn s
         replace f t = T.intercalate t . T.splitOn f
