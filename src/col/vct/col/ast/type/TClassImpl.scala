package vct.col.ast.`type`

import vct.col.ast.{Class, TClass, Type, Variable}
import vct.col.print.{Ctx, Doc, Empty, Group, Text}
import vct.col.ast.ops.TClassOps

trait TClassImpl[G] extends TClassOps[G] { this: TClass[G] =>
  def transSupportArrows: Seq[(Class[G], Class[G])] = cls.decl.transSupportArrows

  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(cls)) <> (
    if (typeArgs.nonEmpty) Text("<") <> Doc.args(typeArgs) <> ">" else Empty)

  def typeEnv: Map[Variable[G], Type[G]] = cls.decl.typeArgs.zip(typeArgs).toMap
}