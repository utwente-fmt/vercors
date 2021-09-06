package vct.col.rewrite

import vct.col.ast.`type`.{ASTReserved, PrimitiveSort, Type}
import vct.col.ast.expr.{Dereference, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.{DeclarationStatement, ProgramUnit}
import vct.col.ast.util.{AbstractRewriter, SequenceUtils}

import scala.collection.mutable

class DesugarValidPointer(source: ProgramUnit) extends AbstractRewriter(source) {
  override def visit(exp: OperatorExpression): Unit = {
    exp.operator match {
      case StandardOperator.ValidPointer =>
        val t = exp.arg(0).getType
        val array = rewrite(exp.arg(0))
        val size = rewrite(exp.arg(1))
        val perm = rewrite(exp.arg(2))
        result = validPointerFor(array, t, size, perm)
      case StandardOperator.ValidPointerIndex =>
        val t = exp.arg(0).getType
        val array = rewrite(exp.arg(0))
        val index = rewrite(exp.arg(1))
        val perm = rewrite(exp.arg(2))
        result = validPointerIndexFor(array, t, index, perm)
      case _ =>
        super.visit(exp)
    }
  }

  def validPointerFor(input: ASTNode, t: Type, size: ASTNode, perm: ASTNode): ASTNode = {
    val conditions: mutable.ListBuffer[ASTNode] = mutable.ListBuffer()
    val seqInfo = SequenceUtils.expectArrayType(t, "Expected an array type here, but got %s")

    if(!seqInfo.isOpt || !seqInfo.isCell) {
      Fail("Expected a pointer type here, but got %s", t)
    }

    var value = input

    conditions += neq(value, create.reserved_name(ASTReserved.OptionNone))
    conditions += lte(size, create.array_length_dereference(value))

    conditions += create.starall(
      and(lte(constant(0), name("__i")), less(name("__i"), size)),
      create.expression(StandardOperator.Perm,
        create.pattern(create.expression(StandardOperator.Subscript, value, name("__i"))),
        perm),
      List(new DeclarationStatement("__i", create.primitive_type(PrimitiveSort.Integer))):_*
    )

    conditions.reduce(star)
  }

  def validPointerIndexFor(input: ASTNode, t: Type, index: ASTNode, perm: ASTNode): ASTNode = {
    val conditions: mutable.ListBuffer[ASTNode] = mutable.ListBuffer()
    val seqInfo = SequenceUtils.expectArrayType(t, "Expected an array type here, but got %s")

    if(!seqInfo.isOpt || !seqInfo.isCell) {
      Fail("Expected a pointer type here, but got %s", t)
    }

    var value = input
    conditions += neq(value, create.reserved_name(ASTReserved.OptionNone))
    conditions += less(index, create.array_length_dereference(value))

    conditions += create.expression(StandardOperator.Perm,
      create.expression(StandardOperator.Subscript, value, index),
      perm)

    conditions.reduce(star)
  }
}
