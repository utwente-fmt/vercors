package vct.col.ast.lang

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ref.Ref

trait CodeStringPredicateConstructorImpl[G] {
  this: CodeStringPredicateConstructor[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("predicate")

}