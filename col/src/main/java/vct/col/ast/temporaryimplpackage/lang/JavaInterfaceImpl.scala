package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{JavaInterface, Type}

trait JavaInterfaceImpl { this: JavaInterface =>
  override def supports: Seq[Type] = ext
}