package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CodeStringPredicateConstructorOps

trait CodeStringPredicateConstructorImpl[G] extends CodeStringPredicateConstructorOps[G] {
  this: CodeStringPredicateConstructor[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("predicate")

}