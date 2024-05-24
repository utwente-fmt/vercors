package vct.col.print

import vct.col.ast.{AccountedPredicate, Expr, Type, Variable}
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers

object DocUtil {
  def splitClauses(e: Expr[_])(implicit ctx: Ctx): Seq[Doc] =
    AstBuildHelpers.unfoldStar(e).map(_.show)

  def splitClauses(e: AccountedPredicate[_])(implicit ctx: Ctx): Seq[Doc] =
    AstBuildHelpers.unfoldPredicate(e).flatMap(AstBuildHelpers.unfoldStar).map(_.show)

  def clauses(key: String, e: Expr[_])(implicit ctx: Ctx): Doc =
    Doc.stack(splitClauses(e).map(Text(key) <+> _ <> (if(ctx.syntax == Ctx.Silver) "" else ";")))

  def clauses(key: String, e: AccountedPredicate[_])(implicit ctx: Ctx): Doc =
    Doc.stack(splitClauses(e).map(Text(key) <+> _ <> (if(ctx.syntax == Ctx.Silver) "" else ";")))

  def givenYieldsMapping(keyword: String, mapping: Seq[(Doc, Doc)])(implicit ctx: Ctx): Doc =
    if(mapping.isEmpty) Empty
    else Nest(Line <> Group(Text(keyword) <+> "{" <> Doc.args(mapping.map(p => p._1 <+> "=" <+> p._2)) <> "}"))

  def givenYields[G](given: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Expr[G], Ref[G, Variable[G]])])(implicit ctx: Ctx): Doc =
    Doc.inlineSpec(Show.lazily(givenYieldsMapping("given", given.map { case (ref, e) => Text(ctx.name(ref)) -> e.show })(_))) <>
      Doc.inlineSpec(Show.lazily(givenYieldsMapping("yields", yields.map { case (e, ref) => e.show -> Text(ctx.name(ref)) })(_)))

  def argsOutArgs[G](args: Seq[Expr[G]], outArgs: Seq[Expr[G]])(implicit ctx: Ctx): Doc = outArgs match {
    case Seq() => Doc.args(args)
    case _ => Doc.args(args) <> ";" <+> Doc.args(outArgs)
  }

  def javaGenericArgs[G](args: Seq[Type[G]])(implicit ctx: Ctx): Doc = args match {
    case Seq() => Empty
    case _ => Text("<") <> Doc.args(args) <> ">"
  }

  def javaGenericParams[G](args: Seq[Variable[G]])(implicit ctx: Ctx): Doc = args match {
    case Seq() => Empty
    case _ => Text("<") <> Doc.args(args) <> ">"
  }
}
