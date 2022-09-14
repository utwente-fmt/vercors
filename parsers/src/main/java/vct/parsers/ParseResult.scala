package vct.parsers

import vct.col.ast.{GlobalDeclaration, VerificationContext}
import vct.col.util.ExpectedError

case object ParseResult {
  def reduce[G](parses: Seq[(ParseResult[G], Option[Language])]): (ParseResult[G], Option[Language]) =
    parses.reduceOption((l, r) => (l, r) match {
      case ((ParseResult(declsLeft, expectedLeft), l1), (ParseResult(declsRight, expectedRight), l2)) =>
        val lan = if(l1 == l2) l1 else None
        (ParseResult(declsLeft ++ declsRight, expectedLeft ++ expectedRight), lan)
    }).getOrElse((ParseResult(Nil, Nil), None))
}

case class ParseResult[G](decls: Seq[GlobalDeclaration[G]], expectedErrors: Seq[ExpectedError])