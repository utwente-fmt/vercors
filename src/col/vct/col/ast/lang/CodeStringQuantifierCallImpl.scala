package vct.col.ast.lang

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ref.Ref

trait CodeStringQuantifierCallImpl[G] {
  this: CodeStringQuantifierCall[G] =>
  override def givenMap: Seq[(Ref[G, Variable[G]], Expr[G])] = Seq.empty
  override def yields: Seq[(Expr[G], Ref[G, Variable[G]])] = Seq.empty
  override def typeArgs: Seq[Type[G]] = Seq.empty
  override def outArgs: Seq[Expr[G]] = Seq.empty


  override def layout(implicit ctx: Ctx): Doc =
    Group(Group(assoc(obj) <> "." <> ctx.name(ref) <> "(" <> Doc.args(args ++ outArgs) <> ")"))

}