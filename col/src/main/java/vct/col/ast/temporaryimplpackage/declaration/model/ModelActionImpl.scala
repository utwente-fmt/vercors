package vct.col.ast.temporaryimplpackage.declaration.model

import vct.col.ast.{ModelAction, Node, TBool, TProcess, Type}
import vct.col.check.{CheckContext, CheckError}

trait ModelActionImpl[G] { this: ModelAction[G] =>
  override def returnType: Type[G] = TProcess()
  override def body: Option[Node[G]] = None

  override def check(context: CheckContext[G]): Seq[CheckError] =
    requires.checkSubType(TBool()) ++ ensures.checkSubType(TBool())
}