package vct.col.ast.statement.terminal

import vct.col.ast.{Expr, NonNullOperation, TNull}
import vct.col.ast.node.NodeImpl
import vct.col.check.{CheckContext, CheckError, TypeErrorExplanation}

trait NonNullOperationImpl[G] extends NodeImpl[G] {
  this: NonNullOperation[G] =>

  def obj: Expr[G]

  override def check(context: CheckContext[G]): Seq[CheckError] =
    obj.t match {
      case TNull() =>
        Seq(TypeErrorExplanation(
          obj,
          "This expression is statically known to be null but it may not be null",
        ))
      case _ => Nil
    }
}
