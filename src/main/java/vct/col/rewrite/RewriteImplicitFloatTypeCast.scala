package vct.col.rewrite

import vct.col.ast.expr.OperatorExpression
import vct.col.ast.stmt.decl.{DeclarationStatement, ProgramUnit}
import vct.col.ast.util.AbstractRewriter
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.terminal.AssignmentStatement

import scala.jdk.CollectionConverters.SeqHasAsJava

class RewriteImplicitFloatTypeCast(source: ProgramUnit) extends AbstractRewriter(source) {
  override def visit(expr: OperatorExpression): Unit = {
    val op = expr.operator
    op match {
      case Plus | Minus | Mult | Div | EQ | NEQ | GT | GTE | LT | LTE =>
        val first = expr.first
        val second = expr.second
        val args =
          if (first.getType.isFloat && second.getType.isInteger) List(rewrite(first), create.expression(IntegerToFloat, rewrite(second))).asJava
          else if (first.getType.isInteger && second.getType.isFloat) List(create.expression(IntegerToFloat, rewrite(first)), rewrite(second)).asJava
          else if (first.getType.isDouble && second.getType.isInteger) List(rewrite(first), create.expression(IntegerToDouble, rewrite(second))).asJava
          else if (first.getType.isInteger && second.getType.isDouble) List(create.expression(IntegerToDouble, rewrite(first)), rewrite(second)).asJava
          else if (first.getType.isFloat && second.getType.isDouble) List(create.expression(FloatToDouble, rewrite(first)), rewrite(second)).asJava
          else if (first.getType.isDouble && second.getType.isFloat) List(rewrite(first), create.expression(FloatToDouble, rewrite(second))).asJava
          else (for (arg <- expr.args) yield rewrite(arg)).asJava

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

  override def visit(s: DeclarationStatement): Unit = {
    s.init match {
      case Some(expr) =>
        val newExpr =
          if (s.getType.isFloat && expr.getType.isInteger) create.expression(IntegerToFloat, rewrite(expr))
          else if (s.getType.isDouble && expr.getType.isInteger) create.expression(IntegerToDouble, rewrite(expr))
          else if (s.getType.isFloat && expr.getType.isDouble) create.expression(DoubleToFLoat, rewrite(expr))
          else if (s.getType.isDouble && expr.getType.isFloat) create.expression(FloatToDouble, rewrite(expr))
          else rewrite(expr)
        result = create.field_decl(s.name, s.getType, newExpr)
      case _ =>
    }

    if (result == null) {
      super.visit(s)
    }
  }
}
