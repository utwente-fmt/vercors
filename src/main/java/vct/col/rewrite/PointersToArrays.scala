package vct.col.rewrite

import java.util

import vct.col.ast.`type`.{ASTReserved, PrimitiveSort, PrimitiveType, Type}
import vct.col.ast.expr.constant.StructValue
import vct.col.ast.expr.{NameExpression, NameExpressionKind, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.{DeclarationStatement, Method, ProgramUnit}
import vct.col.ast.util.AbstractRewriter

import scala.collection.JavaConverters._

class PointersToArrays(source: ProgramUnit) extends AbstractRewriter(source) {
  def visitType(t: Type): Type = {
    if(t.isPrimitive(PrimitiveSort.Pointer)) {
      create.primitive_type(PrimitiveSort.Option,
        create.primitive_type(PrimitiveSort.Array,
          create.primitive_type(PrimitiveSort.Cell,
            visitType(t.firstarg.asInstanceOf[Type]))))
    } else if(t.isInstanceOf[PrimitiveType]) {
      create.primitive_type(t.asInstanceOf[PrimitiveType].sort, t.args.map((node: ASTNode) =>
        node match {
          case t: Type => visitType(t)
          case other => other
        }):_*)
    } else {
      t
    }
  }

  override def visit(decl: DeclarationStatement): Unit = {
    result = DeclarationStatement(
      decl.name,
      visitType(decl.`type`),
      decl.init.map(rewrite(_))
    )

    result.setOrigin(decl.getOrigin)
  }

  override def visit(expr: OperatorExpression): Unit = {
    val args = rewrite(expr.args.asJava)
    result = expr.operator match {
      case StandardOperator.AddrOf =>
        if(args.get(0).isa(StandardOperator.Subscript)) {
          val subscript = args.get(0).asInstanceOf[OperatorExpression]
          create.expression(StandardOperator.Drop, subscript.arg(0), subscript.arg(1))
        } else {
          throw Failure("Argument to AddrOf (&) was not a pointer at %s", expr.getOrigin)
        }
      case StandardOperator.Indirection =>
        create.expression(StandardOperator.Subscript, args.get(0), create.constant(0))
      case StandardOperator.Plus
        if expr.arg(0).getType.isPrimitive(PrimitiveSort.Pointer)
          || (expr.arg(0).getType.isPrimitive(PrimitiveSort.Option) && expr.arg(0).getType.firstarg.asInstanceOf[Type].isPrimitive(PrimitiveSort.Array)) =>
        create.expression(StandardOperator.Drop, args.get(0), args.get(1))
      case otherOp =>
        create.expression(otherOp, args)
    }
  }

  override def visit(value: StructValue): Unit = {
    result = create.struct_value(
      visitType(value.`type`),
      value.map.asJava,
      rewrite(value.values.asJava)
    )
  }

  override def visit(reserved: NameExpression): Unit = {
    if(reserved.kind != NameExpressionKind.Reserved) {
      super.visit(reserved)
    } else {
      reserved.reserved match {
        case ASTReserved.Null if reserved.getType.isPrimitive(PrimitiveSort.Pointer) =>
          result = create reserved_name(ASTReserved.OptionNone)
        case _ =>
          super.visit(reserved)
      }
    }
  }
}
