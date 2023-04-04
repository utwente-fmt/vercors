package vct.col.ast.expr.binder

import vct.col.ast.{Exists, TBool, Type}
import vct.col.print.{Ctx, Doc, Group, Line, Nest, Precedence, Text}

trait ExistsImpl[G] { this: Exists[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Group(
    Text("(\\exists") <+> Doc.fold(bindings)(_ <> ", " <> _) <> ";" <>>
      { body } </>
    ")"
  )
}