{-# OPTIONS_GHC -Wall -Wno-overlapping-patterns -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wpartial-fields -Wredundant-constraints #-}
{-# LANGUAGE TemplateHaskell #-}
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
data SepBy a b = Sepd a (Maybe (b, (SepBy a b))) deriving (Show,Eq,Ord,Generic)

type JavaFile = SepBy TextWithComments ClassDescription
type ClassContent = SepBy TextWithComments ClassItem
type SpacesWithComments = TextWithComments

-- the first textwithcomments is for the 'class X ' itself.
-- the classcontent is surrounded by {}
data ClassDescription
    = ClassDescription TextWithComments ClassContent deriving (Show,Eq,Ord,Generic)

data Contract = Contract [Condition] deriving (Show,Eq,Ord,Generic)

data Condition = Condition CondKeyword Balanced SpacesWithComments deriving (Show,Eq,Ord,Generic)

data CondKeyword
    = Requires | Ensures | Invariant | Context | Given | Yields deriving (Show,Eq,Ord,Generic)

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

 -- MethodHeader indicates the function name and arguments
data MethodHeader = MethodHeader TextWithComments TextWithComments SpacesWithComments
   deriving (Show,Eq,Ord,Generic) -- type and name, arguments, trailing whitespace
data ClassItem
    = Method Contract MethodHeader DefinitionExpr
    | Declaration Balanced
    | NoItem T.Text -- just whitespace (no ;). Should not occur in normalised ClassContent.
    deriving (Show,Eq,Ord,Generic)
data DefinitionExpr
    = NoDefinition -- just a semicolon. Has the semantics of the AssumeFalse but Cannot be used everywhere
    | Sequential Balanced -- ordinary {}-wrapped code block
    | Mathematical Balanced -- something starting with =
    | AssumeFalse Int -- {assume false; with a bunch of newlines}
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
