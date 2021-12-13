package vct.col.ast.temporaryimplpackage.declaration.model

import vct.col.ast.{ModelProcess, Node, TBool, TProcess, Type}
import vct.col.check.{CheckContext, CheckError}

trait ModelProcessImpl[G] { this: ModelProcess[G] =>
  override def returnType: Type[G] = TProcess()
  override def body: Option[Node[G]] = Some(impl)
  override def check(context: CheckContext[G]): Seq[CheckError] =
    impl.checkSubType(TProcess()) ++ requires.checkSubType(TBool()) ++ ensures.checkSubType(TBool())
}