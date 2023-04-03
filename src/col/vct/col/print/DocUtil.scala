package vct.col.print

import vct.col.ast.{AccountedPredicate, Expr}
import vct.col.util.AstBuildHelpers

object DocUtil {
  def splitClauses(e: Expr[_])(implicit ctx: Ctx): Seq[Doc] =
    AstBuildHelpers.unfoldStar(e).map(_.show)

  def splitClauses(e: AccountedPredicate[_])(implicit ctx: Ctx): Seq[Doc] =
    AstBuildHelpers.unfoldPredicate(e).map(_.show)

  def clauses(key: String, e: Expr[_])(implicit ctx: Ctx): Doc =
    Doc.stack(splitClauses(e).map(Text(key) <+> _ <> ";"))

  def clauses(key: String, e: AccountedPredicate[_])(implicit ctx: Ctx): Doc =
    Doc.stack(splitClauses(e).map(Text(key) <+> _ <> ";"))
}
