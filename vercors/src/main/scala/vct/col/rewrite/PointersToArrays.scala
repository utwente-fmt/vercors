package vct.col.rewrite

import hre.lang.System.Failure
import vct.col.ast.`type`.{PrimitiveSort, PrimitiveType, Type}
import vct.col.ast.expr.constant.StructValue
import vct.col.ast.expr.{OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.{DeclarationStatement, Method, ProgramUnit}

import scala.collection.JavaConversions._

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

  override def visit(method: Method): Unit = {
    result = new Method(
      method.kind,
      method.name,
      visitType(method.getReturnType),
      rewrite(method.getContract),
      rewrite(method.getArgs),
      method.usesVarArgs,
      rewrite(method.getBody),
    )

    result.setOrigin(method.getOrigin)
  }

  override def visit(expr: OperatorExpression): Unit = {
    val args = rewrite(expr.args)
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
        create.expression(otherOp, args:_*)
    }
  }

  override def visit(value: StructValue): Unit = {
    result = create.struct_value(
      visitType(value.`type`),
      value.map,
      rewrite(value.values)
    )
  }
}
