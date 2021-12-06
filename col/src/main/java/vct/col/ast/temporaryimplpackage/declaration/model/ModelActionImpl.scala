package vct.col.ast.temporaryimplpackage.declaration.model

import vct.col.ast.{ModelAction, Node, TBool, TProcess, Type}
import vct.col.check.{CheckContext, CheckError}

trait ModelActionImpl { this: ModelAction =>
  override def returnType: Type = TProcess()
  override def body: Option[Node] = None

  override def check(context: CheckContext): Seq[CheckError] =
    requires.checkSubType(TBool()) ++ ensures.checkSubType(TBool())
}