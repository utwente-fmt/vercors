package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Text}

trait CodeStringPredicateConstructorImpl[G] {
  this: CodeStringPredicateConstructor[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("predicate")

}