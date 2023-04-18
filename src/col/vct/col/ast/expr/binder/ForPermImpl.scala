package vct.col.ast.expr.binder

import vct.col.ast.{ForPerm, TBool}
import vct.col.print._

trait ForPermImpl[G] { this: ForPerm[G] =>
  override def t: TBool[G] = TBool()

  def layoutSpec(implicit ctx: Ctx): Doc =
    Group(Text("(\\forperm") <+> Doc.fold(bindings)(_ <> "," <+> _) <+> "\\in" <+> loc <> ";" <>> body.show </> ")")

  // PB: does not need parentheses, so perhaps make precedence depend on ctx
  def layoutSilver(implicit ctx: Ctx): Doc =
    Group(Text("(forperm") <+> Doc.fold(bindings)(_ <> "," <+> _) <+> "[" <> loc <> "]" <+> "::" <>> body.show </> ")")

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = ctx.syntax match {
    case Ctx.Silver => layoutSilver
    case _ => layoutSpec
  }
}
