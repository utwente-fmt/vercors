{-# OPTIONS_GHC -Wall -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wpartial-fields -Wredundant-constraints #-}
{-# LANGUAGE TypeOperators, OverloadedStrings, NoImplicitPrelude #-}
module SplitVerify.JavaParse (javaFile) where
import qualified RIO.Text as T
import RIO hiding ((.), id)
import qualified RIO.Char as C
import Control.Category ((.), id)
import Text.Boomerang (Boomerang(..),(:-),Parser(..),ErrorMsg(..)
                      ,rCons,rNil,duck1,xmaph,mkParserError,incMajor,incMinor,val,xpure
                      ,(<?>),rNothing,rJust,rPair,push,(:-)(..))
import Text.Boomerang.String hiding (lit)
import SplitVerify.JavaStructures

-- this file describes all bi-directional parsers
-- a bidirectional parser 'StringBoomerang a b' can be seen as two transformations:
--   a to [Either e b], where the side-effect is consuming characters (i.e. parsing)
--   b to [a], where the side-effect is producing characters (i.e. printing)
-- in the direction from right to left, errors can be produced (for parse-errors).
-- there are of type e in the above, but actually StringError for StringBoomerang.
-- If the error type is polymorhpic (i.e. Boomerang e _ _ _ with e free), no errors can occur.

-- I would have prefered the two transformations to have these types:
--   a to Either e1 [b]
--   b to Either e2 [a]
-- However, that would require rewriting the entire boomerang library.
-- That would mean adding a library that needs to be maintained, I decided to work with what is there instead.

javaFile :: StringBoomerang r (JavaFile :- r) -- r for 'remainder of the stack'
javaFile = rListSepBy1 (textOrCommentNullable C.isSpace) classDescription

classDescription :: StringBoomerang r (ClassDescription :- r)
classDescription = (textWithComments (\x -> x /= '{' && x /= '}') >>> rClassDescription) . classContent

classContent :: StringBoomerang r (SepBy [TextOrComment] ClassItem :- r)
classContent = "{" . rListSepBy (textOrCommentNullable C.isSpace) classItem . "}"

contract :: StringBoomerang r (Contract :- r)
contract = rContract . rList condition <?> "Contract"

condition :: StringBoomerang r (Condition :- r)
condition = condKeyword 
          . balanced (\x -> x /= ';' && x /= '{' && x /= '}')
          . ";"
          . spacesWithComments
          >>> rCondition

classItem :: StringBoomerang r (ClassItem :- r)
classItem =  (((contract . methodHeader . definitionExpr >>> rMethod) <?> "Contract with method body")
          <.> (balanced (\x -> x /= ';' && x/='{' && x/='}') . ";" >>> rDeclaration)) <?> "Class item"

definitionExpr :: StringBoomerang r (DefinitionExpr :- r)
definitionExpr = ";" . rNoDefinition
               <.> "=" . (balanced (\x -> x /= ';') . ";" >>> rMathematical)
               <.> (bracketedSt >>> rSequential)
               <.> "{/*@ assume false;" . rAssumeFalse . countNewlines . "@*/}"
               <?> "Method body or definition (or semi-colon)"

countNewlines :: StringBoomerang r (Int :- r)
countNewlines = "\n" . xmaph ((+) (1::Int)) (\x -> if x > 0 then Just $ x - 1 else Nothing) countNewlines
              <.> push 0
               
condKeyword :: StringBoomerang r (CondKeyword :- r)
condKeyword
 =   "ensures"   . rEnsures
 <.> "requires"  . rRequires
 <.> "invariant" . rInvariant
 <.> "context"   . rContext
 <.> "given"     . rGiven
 <.> "yields"    . rYields

spacesWithComments :: StringBoomerang r (SpacesWithComments :- r)
spacesWithComments = textOrCommentNullable C.isSpace

methodHeader :: StringBoomerang r (MethodHeader :- r)
methodHeader
 = look1 (not . C.isSpace) . (textWithComments noBrac) . "(" . textOrCommentNullable noBrac . ")"
   . spacesWithComments
   >>> rMethodHeader
   <?> "Method or function declaration"
 where noBrac = (\x -> x /= ';' && x /= '=' && x /= '(' && x /= ')' && x /= '{' && x /='}')

-- Parsing text and brackets

balancedPart :: (Char -> Bool) -> StringBoomerang r (BalancedPart :- r)
balancedPart r = (bracketed >>> rBracketedRound)
              <.> (bracketedSt >>> rBracketedStach)
              <.> (textOrComment (\x -> r x && x /= '(' && x /= ')' && x /= '{' && x /= '}') >>> rNoBrackets)
balanced, balancedNullable :: (Char -> Bool) -> StringBoomerang r (Balanced :- r)
balanced r = rList1 (balancedPart r)
balancedNullable r = balanced r <.> rNil
bracketed, innerBrackets, bracketedSt :: StringBoomerang r (Balanced :- r)
bracketed = "(" . innerBrackets . ")"
bracketedSt = "{" . innerBrackets . "}"
innerBrackets = balancedNullable (const True)

textWithComments, textOrCommentNullable :: (Char -> Bool) -> StringBoomerang r ([TextOrComment] :- r)
textWithComments r = rList1 (textOrComment r)
textOrCommentNullable r = rList (textOrComment r)

textOrComment :: (Char -> Bool) -> StringBoomerang r (TextOrComment :- r)
textOrComment r
  = rTextNoComment . toText . textNoComment
   <.> "//" . rTextCommentLine . noNewlines . newlines
   <.> "/*" . rTextCommentInLine . toText . commentInline . "*/"
 where
    noNewlines = rText (satisfy' (\x -> x /= '\n' && x /= '\r') "newline-less string")
    newlines = rText1 (satisfy' (\x -> x == '\n' || x == '\r') "Newlines at end of comment")
    commentInline,textNoComment :: StringBoomerang r ([Char] :- r)
    commentInline
     = lookN (\x -> take 2 x /= "*/") . rCons . satisfy (const True) . (commentInline <.> rNil)
    textNoComment
      = (     prefix "/*@"
          <.> prefix "@*/"
          <.> prefix "//@"
          <.> lookN (\v -> let v2 = take 2 v
                           in v2 /= "*/" && v2 /="//" && v2 /="/*")
              . (satisfy' (\x -> r x) "something part of not-a-comment" >>> rCons)
        ) . (textNoComment <.> rNil)

-- helper functions

-- | prefix acts like fromString, but additionally pushes the parsed string onto the stack
prefix :: [Char]
       -> Boomerang StringError String ([Char] :- r1) ([Char] :- r1)
prefix str
 = fromString str .
   xpure (\(lst :- r) -> (str++lst) :- r)
         (\(lst :- r) -> if take (length str) lst == str
                         then Just (drop (length str) lst :- r)
                         else Nothing)

-- | Converts a router for a value @a@ to a router for a sepby-list of @a@, with a separator.
rListSepBy, rListSepBy1 :: (Show e,Show tok)
           => (forall r. Boomerang e tok r (a :- r)) -> (forall r. Boomerang e tok r (b :- r))
           -> Boomerang e tok r2 (SepBy a b :- r2)
rListSepBy  a b = a . ((b >>> rJust . rPair) . rListSepBy a b <.> rNothing) >>> rSepd
rListSepBy1 a b = a . (b . rListSepBy a b >>> rJust . rPair) >>> rSepd -- needs at least one b-occurrence

-- | Converts a router for a value @a@ to a router for a list of @a@.
rList, rList1 :: Boomerang e tok r (a :- r) -> Boomerang e tok r ([a] :- r)
rList  r = manyr (duck1 r >>> rCons) . rNil
rList1 r = somer (duck1 r >>> rCons) . rNil -- needs at least one occurrence

-- | Repeat a router zero or more times, combining the results from left to right.
manyr,somer :: Boomerang e tok r r -> Boomerang e tok r r
manyr = (<.> id) . somer
-- | Repeat a router one or more times, combining the results from left to right.
somer p = p . manyr p

lookN :: (String -> Bool) -> StringBoomerang r r
lookN r = Boomerang
  (Parser $ \tok pos ->
      if r tok then [Right ((id,tok), pos)]
               else mkParserError pos [UnExpect "something in lookN call"]
  )
  (\str -> [(id, str)]) -- [ id | null str ])
 
look1 :: (Char -> Bool) -> StringBoomerang r r
look1 r = Boomerang
  (Parser $ \tok pos ->
       case tok of
         []    -> [] -- Right ((id,tok), pos)]
         (h:_) -> if r h then [Right ((id,tok), pos)]
                    else mkParserError pos [UnExpect ("Character: "<>show h)]
  )
  (\str -> [(id, str)]) -- [ id | null str ])

-- | statisfy a 'Char' predicate and say what you expect
satisfy' :: (Char -> Bool) -> String -> StringBoomerang r (Char :- r)
satisfy' p e = val
  (Parser $ \tok pos ->
    case tok of
      [] -> mkParserError pos [EOI "input",Expect e]
      (c:cs) | p c -> [Right ((c, cs), if (c == '\n') then incMajor (1::Int) pos else incMinor (1::Int) pos)]
             | otherwise -> mkParserError pos [SysUnExpect $ show c,Expect e])
  (\c -> [ \paths -> (c:paths) | p c ])

-- Committing choice: if first parse succeeds, do not consider the second option.
-- Using this makes parsers a lot faster, but it violates bidirectionality in some cases.
-- Parsers that can inspect the stack should be avoided after using this choice, consider:
-- (a <.> b) . c
-- where c inspects a result of a or b and might fail on all of a (but not on something of b)
-- A solution is to replace such patterns by distribution:
-- (a . c <.> b . c) will succeed if (a <|> b) . c does.
-- Whereas (a <|> b) . c = (a . c <|> b . c), this property only holds for <.> if the router doesn't change its behavior based on the stack.
infixl 6 <.>
(<.>) :: Boomerang e tok a b -> Boomerang e tok a b -> Boomerang e tok a b
(Boomerang pf sf) <.> ~(Boomerang pg sg)
  = Boomerang (pf `mChoice` pg)
              (\s -> case (sf s) of [] -> (sg s); v -> v)
  where mChoice :: Parser e tok a -> Parser e tok a -> Parser e tok a
        mChoice (Parser f) (Parser g)
         = Parser $ \tok pos -> let vs = f tok pos
                                in case filter isRight vs of [] -> (g tok pos) ; x -> x

toText :: Boomerang e tok (String :- o) (Text :- o)
toText = xmaph T.pack (Just . T.unpack) id

rText,rText1 :: StringBoomerang r (Char :- r) -> StringBoomerang r (Text :- r)
rText v = rList v >>> toText
rText1 v = rList1 v >>> toText
