package vct.col.ast.expr.binder

import vct.col.ast.{Forall, TBool, Type}
import vct.col.print._

trait ForallImpl[G] { this: Forall[G] =>
  override def t: Type[G] = TBool()

  override def layout(implicit ctx: Ctx): Doc = Group(
    Text("(\\forall") <+> Doc.fold(bindings)(_ <> ", " <> _) <> ";" <>
      Nest(Line <> body) </>
    ")"
  )
}
