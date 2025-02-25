package vct.col.ast.expr.binder

import vct.col.ast.{Let, Type, Variable}
import vct.col.print._
import vct.col.ast.ops.LetOps

trait LetImpl[G] extends LetOps[G] {
  this: Let[G] =>
  override def t: Type[G] = main.t
  override def bindings: Seq[Variable[G]] = Seq(binding)

  def layoutSpec(implicit ctx: Ctx): Doc =
    Group(
      Text("(") <> "\\let" <+> binding <+> "=" <+> value <> ";" <>> main </> ")"
    )

  def layoutSilver(implicit ctx: Ctx): Doc =
    Group(
      Text("(") <> "let" <+> ctx.name(binding) <+> "==" <+> "(" <> value <>
        ")" <+> "in" <>> main </> ")"
    )

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => layoutSilver
      case _ => layoutSpec
    }
}
