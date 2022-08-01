package vct.col.ast.temporaryimplpackage.expr.misc
import vct.col.ast.{OldLabeled, Type}


trait OldLabeledImpl[G] { this: OldLabeled[G] =>
  override def t: Type[G] = expr.t

}
