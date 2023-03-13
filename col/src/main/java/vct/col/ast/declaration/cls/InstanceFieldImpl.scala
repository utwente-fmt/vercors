package vct.col.ast.declaration.cls

import vct.col.ast.{Final, InstanceField}

trait InstanceFieldImpl[G] { this: InstanceField[G] =>
  def isFinal = flags.collectFirst { case _: Final[G] => () }.isDefined
}