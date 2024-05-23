package vct.col.ast.expr.binder

import vct.col.ast.{Starall, TResource, Type}
import vct.col.print._
import vct.col.ast.ops.StarallOps

trait StarallImpl[G] extends StarallOps[G] { this: Starall[G] =>
  override def t: Type[G] = TResource()

  def layoutTriggers(implicit ctx: Ctx): Doc =
    Doc.fold(triggers.map { trig =>
      Nest(Line <> Group(Text("{") <> Doc.args(trig) <> "}"))
    })(_ <> _)

  def layoutSpec(implicit ctx: Ctx): Doc =
    Group(Text("(\\forall*") <+> Doc.fold(bindings)(_ <> "," <+> _) <> ";" <>> body </> ")")

  def layoutSilver(implicit ctx: Ctx): Doc =
    Group(Text("(forall") <+> Doc.fold(bindings)(_ <> "," <+> _) <+> "::" <> layoutTriggers <>> body </> ")")

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = ctx.syntax match {
    case Ctx.Silver => layoutSilver
    case _ => layoutSpec
  }
}
