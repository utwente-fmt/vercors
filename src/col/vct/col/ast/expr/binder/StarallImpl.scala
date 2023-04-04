package vct.col.ast.expr.binder

import vct.col.ast.{Starall, TResource, Type}
import vct.col.print._

trait StarallImpl[G] { this: Starall[G] =>
  override def t: Type[G] = TResource()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Group(
    Text("(\\forall*") <+> Doc.fold(bindings)(_ <> ", " <> _) <> ";" <>>
      { body } </>
    ")"
  )
}
