package vct.col.rewrite

import vct.col.ast.`type`.{ClassType, PrimitiveSort, PrimitiveType, Type}
import vct.col.ast.stmt.decl.{ASTClass, ProgramUnit, TypeAlias}
import hre.lang.System.Output
import vct.col.ast.expr.{NameExpression, OperatorExpression, StandardOperator}

class RewriteRecordPointers(source: ProgramUnit) extends AbstractRewriter(source) {
  override def visit(t: PrimitiveType): Unit = {
    t.sort match {
      case PrimitiveSort.Pointer =>
        t.firstarg match {
          case classType: ClassType =>
            val definition = source.find_decl(classType.getNameFull)
            if (definition.asInstanceOf[ASTClass].kind == ASTClass.ClassKind.Record) {
              result = classType
            } else {
              super.visit(t)
            }
          case _ => super.visit(t)
        }
      case _ =>
        super.visit(t)
    }
  }

  override def visit(alias: TypeAlias): Unit = {
    // We copy type aliases, as we can safely allow bare record types in type aliases. Only in resolved types we cannot
    // allow bare record types.
    result = copy_rw.rewrite(alias)
  }

  override def visit(t: ClassType): Unit = {
    source.find_immediate_decl(t.getNameFull) match {
      case alias: TypeAlias =>
        result = rewrite(alias.aliasedType)
      case cls: ASTClass if cls.kind == ASTClass.ClassKind.Record =>
        Fail("%s", "Unsupported: cannot refer to a struct without indirection")
      case _ =>
        super.visit(t)
    }
  }

  override def visit(o: OperatorExpression): Unit = o.operator match {
    case StandardOperator.StructDeref =>
      result = create.dereference(rewrite(o.arg(0)), o.arg(1).asInstanceOf[NameExpression].getName)
    case _ =>
      super.visit(o)
  }
}
