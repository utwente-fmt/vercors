package vct.col.ast.lang

import vct.col.ast.CPPAddressingDeclarator
import vct.col.print.{Ctx, Doc}

trait CPPAddressingDeclaratorImpl[G] { this: CPPAddressingDeclarator[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    inner.show <> Doc.fold(operators)(_ <> _)
}