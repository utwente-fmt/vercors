package vct.col.ast.declaration.adt

import vct.col.ast.{ADTAxiom, TBool}
import vct.col.check.{CheckContext, CheckMessage}
import vct.col.print._

trait ADTAxiomImpl[G] { this: ADTAxiom[G] =>
  override def check(context: CheckContext[G]): Seq[CheckMessage] = axiom.checkSubType(TBool())

  override def layout(implicit ctx: Ctx): Doc = ctx.syntax match {
    case Ctx.Silver => Group(Text("axiom") <+> "{" <>> axiom.show <+/> "}")
    case _ => Text("axiom") <+> axiom
  }
}