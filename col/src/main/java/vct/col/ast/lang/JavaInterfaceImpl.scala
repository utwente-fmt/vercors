package vct.col.ast.lang

import vct.col.ast.{JavaInterface, PinnedDecl, Type}

trait JavaInterfaceImpl[G] { this: JavaInterface[G] =>
  override def supports: Seq[Type[G]] = ext
}