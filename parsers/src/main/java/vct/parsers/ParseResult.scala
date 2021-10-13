package vct.parsers

import vct.col.ast.GlobalDeclaration
import vct.col.util.ExpectedError

case object ParseResult {
  def reduce(parses: Seq[ParseResult]): ParseResult =
    parses.reduceOption((l, r) => (l, r) match {
      case (ParseResult(declsLeft, expectedLeft), ParseResult(declsRight, expectedRight)) =>
        ParseResult(declsLeft ++ declsRight, expectedLeft ++ expectedRight)
    }).getOrElse(ParseResult(Nil, Nil))
}

case class ParseResult(decls: Seq[GlobalDeclaration], expectedError: Seq[ExpectedError])
