package vct.col.ast

import RewriteHelpers._
import AstBuildHelpers._
import Constant._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val rewriter: AbstractRewriter = new Rewriter()
    implicit val o: Origin = DiagnosticOrigin
    implicit val blame: DivByZeroBlame = new DivByZeroBlame {
      override def divisionByZero(div: DividingExpr): Unit = println("panic!!")
    }

    val exp = Plus(0, 5)
    println(exp.rewrite(left=IntegerValue(3)))
    val exp2: Expr = 0 + 1 / 2
    println(exp2)
  }
}
