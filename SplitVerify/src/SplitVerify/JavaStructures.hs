{-# OPTIONS_GHC -Wall -Wno-overlapping-patterns -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wpartial-fields -Wredundant-constraints #-}
{-# LANGUAGE TemplateHaskell, TypeOperators, OverloadedStrings, NoImplicitPrelude #-}
module SplitVerify.JavaStructures where
import qualified RIO.Text as T
import RIO
import Text.Boomerang.TH

-- datastructures for Java and Java-ish languages
-- these are in a separate file so we can:
-- 1. share them
-- 2. generate boomerang instances with template-haskell
-- 3. ignore the pattern overlap warnings that the boomerang instances cause without turning those warnings off everywhere

-- | A series of a's, separated by b's. There is at least one a.
data SepBy a b
    = Sepd a (Maybe (b, (SepBy a b))) deriving (Show,Eq,Ord,Generic)

type JavaFile = SepBy TextWithComments ClassDescription
type ClassContent = SepBy TextWithComments ClassItem
type SpacesWithComments = TextWithComments

data ClassDescription
    = ClassDescription TextWithComments -- header
                       ClassContent deriving (Show,Eq,Ord,Generic)

data Contract = Contract [Condition] deriving (Show,Eq,Ord,Generic)

data Condition
    = Condition CondKeyword Balanced SpacesWithComments
    deriving (Show,Eq,Ord,Generic)

data CondKeyword
    = Requires | Ensures | Invariant | Context
    | Given | Yields deriving (Show,Eq,Ord,Generic)

data TextOrComment
    = TextCommentLine T.Text T.Text -- comment and line-end
    | TextCommentInLine T.Text
    | TextNoComment T.Text deriving (Show,Eq,Ord,Generic)
type TextWithComments = [TextOrComment]

data BalancedPart
    = NoBrackets TextOrComment
    | BracketedRound [BalancedPart]
    | BracketedStach [BalancedPart] deriving (Show,Eq,Ord,Generic)
type Balanced = [BalancedPart]

data MethodHeader = MethodHeader TextWithComments TextWithComments SpacesWithComments
   deriving (Show,Eq,Ord,Generic) -- type and name, arguments, trailing whitespace
data ClassItem
    = Method Contract
             MethodHeader -- function name and arguments
             DefinitionExpr
    | Declaration Balanced
    deriving (Show,Eq,Ord,Generic)
data DefinitionExpr
    = NoDefinition
    | Sequential Balanced
    | Mathematical Balanced
    | AssumeFalse Int 
    deriving (Show,Eq,Ord,Generic)

-- generates functions that are the constructors prefixed with an r
-- for example, for NoDefinition, rNoDefinition is generated
$(makeBoomerangs ''SepBy)
$(makeBoomerangs ''ClassDescription)
$(makeBoomerangs ''Contract)
$(makeBoomerangs ''Condition)
$(makeBoomerangs ''CondKeyword)
$(makeBoomerangs ''MethodHeader)
$(makeBoomerangs ''ClassItem)
$(makeBoomerangs ''TextOrComment)
$(makeBoomerangs ''BalancedPart)
$(makeBoomerangs ''DefinitionExpr)
