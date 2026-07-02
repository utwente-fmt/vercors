package vct.col.ast.expr.binder

import vct.col.ast.{LetSuchThat, Type, Variable}
import vct.col.ast.ops.LetSuchThatOps
import vct.col.print._

trait LetSuchThatImpl[G] extends LetSuchThatOps[G] {
  this: LetSuchThat[G] =>
  override def t: Type[G] = main.t
  override def bindings: Seq[Variable[G]] = Seq(binding)

  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Text("(") <> "\\let" <+> binding <+> ":|" <+> value <> ";" <>> main </>
        ")"
    )

  override def precedence: Int = Precedence.ATOMIC
}
