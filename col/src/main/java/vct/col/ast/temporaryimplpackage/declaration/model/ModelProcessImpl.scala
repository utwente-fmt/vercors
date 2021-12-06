package vct.col.ast.temporaryimplpackage.declaration.model

import vct.col.ast.{ModelProcess, Node, TBool, TProcess, Type}
import vct.col.check.{CheckContext, CheckError}

trait ModelProcessImpl { this: ModelProcess =>
  override def returnType: Type = TProcess()
  override def body: Option[Node] = Some(impl)
  override def check(context: CheckContext): Seq[CheckError] =
    impl.checkSubType(TProcess()) ++ requires.checkSubType(TBool()) ++ ensures.checkSubType(TBool())
}