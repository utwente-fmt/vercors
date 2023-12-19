package vct.col.ast.declaration.global

import vct.col.ast.{Node, ProverFunction}

trait ProverFunctionImpl[G] { this: ProverFunction[G] =>
  override def body: None.type = None
}
