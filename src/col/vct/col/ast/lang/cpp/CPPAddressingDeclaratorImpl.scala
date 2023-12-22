package vct.col.ast.lang.cpp

import vct.col.ast.CPPAddressingDeclarator
import vct.col.print.{Ctx, Doc}
import vct.col.ast.ops.CPPAddressingDeclaratorOps

trait CPPAddressingDeclaratorImpl[G] extends CPPAddressingDeclaratorOps[G] { this: CPPAddressingDeclarator[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    inner.show <> Doc.fold(operators)(_ <> _)
}