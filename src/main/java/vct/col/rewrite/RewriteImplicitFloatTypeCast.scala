package vct.col.rewrite

import vct.col.ast.expr.OperatorExpression
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.util.AbstractRewriter
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.generic.ASTNode

import scala.jdk.CollectionConverters.SeqHasAsJava

class RewriteImplicitFloatTypeCast(source: ProgramUnit) extends AbstractRewriter(source) {
  override def visit(expr: OperatorExpression): Unit = {
    val op = expr.operator
    op match {
      case Plus | Minus | Mult | Div | EQ | NEQ | GT | GTE | LT | LTE =>
        val first = expr.first
        val second = expr.second
        var args: java.util.List[ASTNode] = null

        if (first.getType.isFloat && second.getType.isInteger) args = List(rewrite(first), create.expression(IntegerToFloat, rewrite(second))).asJava
        else if (first.getType.isInteger && second.getType.isFloat) args = List(create.expression(IntegerToFloat, rewrite(first)), rewrite(second)).asJava
        else if (first.getType.isDouble && second.getType.isInteger) args = List(rewrite(first), create.expression(IntegerToDouble, rewrite(second))).asJava
        else if (first.getType.isInteger && second.getType.isDouble) args = List(create.expression(IntegerToDouble, rewrite(first)), rewrite(second)).asJava
        else args = (for (arg <- expr.args) yield rewrite(arg)).asJava

        val res = create.expression(op, args)
        res.set_before(rewrite(expr.get_before))
        res.set_after(rewrite(expr.get_after))
        result = res
      case _ =>
    }

    if (result == null) {
      super.visit(expr)
    }
  }
}
