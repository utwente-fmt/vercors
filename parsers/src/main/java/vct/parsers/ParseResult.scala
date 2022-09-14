package vct.parsers

import vct.col.ast.{GlobalDeclaration, VerificationContext}
import vct.col.util.ExpectedError

case object ParseResult {
  def reduce[G](parses: Seq[ParseResult[G]]): ParseResult[G] =
    parses.reduceOption((l, r) => (l, r) match {
      case (ParseResult(declsLeft, expectedLeft), ParseResult(declsRight, expectedRight)) =>
        ParseResult(declsLeft ++ declsRight, expectedLeft ++ expectedRight)
    }).getOrElse(ParseResult(Nil, Nil))
}

case class ParseResult[G](decls: Seq[GlobalDeclaration[G]], expectedErrors: Seq[ExpectedError])