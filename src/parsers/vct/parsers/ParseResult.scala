package vct.parsers

import vct.col.ast.{GlobalDeclaration, VerificationContext}

case object ParseResult {
  def reduce[G](parses: Seq[ParseResult[G]]): ParseResult[G] =
    parses.reduceOption((l, r) => (l, r) match {
      case (ParseResult(declsLeft), ParseResult(declsRight)) =>
        ParseResult(declsLeft ++ declsRight)
    }).getOrElse(ParseResult(Nil))
}

case class ParseResult[G](decls: Seq[GlobalDeclaration[G]])