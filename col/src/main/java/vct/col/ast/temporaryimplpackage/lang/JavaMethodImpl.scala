package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{Declaration, JavaMethod}
import vct.col.ast.temporaryimplpackage.util.Declarator

trait JavaMethodImpl extends Declarator { this: JavaMethod =>
  override def declarations: Seq[Declaration] = parameters ++ typeParameters ++ contract.givenArgs ++ contract.yieldsArgs
}