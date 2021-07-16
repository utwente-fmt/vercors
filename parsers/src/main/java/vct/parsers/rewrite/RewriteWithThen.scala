package vct.parsers.rewrite

import vct.col.ast.expr.{MethodInvokation, NameExpression, OperatorExpression}
import vct.col.ast.generic.{ASTNode, BeforeAfterAnnotations}
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement, ParallelAtomic}
import vct.col.ast.stmt.decl.ASTSpecial.Kind
import vct.col.ast.stmt.decl.{ASTSpecial, ProgramUnit}
import vct.col.ast.util.AbstractRewriter

import scala.jdk.CollectionConverters._

class RewriteWithThen(source: ProgramUnit) extends AbstractRewriter(source) {
  override def visit(invokation: MethodInvokation): Unit = {
    super.visit(invokation)
    result.asInstanceOf[MethodInvokation].clearBefore
    result.asInstanceOf[MethodInvokation].clearAfter
    process_with_then(result.asInstanceOf[MethodInvokation], invokation)
  }

  override def visit(op: OperatorExpression): Unit = {
    super.visit(op)
    result.asInstanceOf[OperatorExpression].clearBefore
    result.asInstanceOf[OperatorExpression].clearAfter
    process_with_then(result.asInstanceOf[OperatorExpression], op)
  }

  override def visit(loop: LoopStatement): Unit = {
    super.visit(loop)
    result.asInstanceOf[BeforeAfterAnnotations].set_before(null)
    result.asInstanceOf[BeforeAfterAnnotations].set_after(null)
    process_with_then(result.asInstanceOf[LoopStatement], loop)
  }

  override def visit(atomic: ParallelAtomic): Unit = {
    super.visit(atomic)
    result.asInstanceOf[BeforeAfterAnnotations].set_before(create.block())
    result.asInstanceOf[BeforeAfterAnnotations].set_after(create.block())
    process_with_then(result.asInstanceOf[ParallelAtomic], atomic)
  }

  private def process_with_then[T <: ASTNode with BeforeAfterAnnotations](dst: T, src: T) = {
    // Keep the before statements
    for (n <- src.get_before.asScala) {
      dst.get_before.add(rewrite(n))
    }

    // Sort the after statements by with / then
    for (n <- src.get_after.asScala) n match {
      case special: ASTSpecial => special.kind match {
        case Kind.Label =>
          dst.labeled(special.args(0).asInstanceOf[NameExpression].getName)
        case Kind.With => for (withMapping <- special.args(0).asInstanceOf[BlockStatement].asScala) {
          dst.get_before().add(rewrite(withMapping))
        }
        case Kind.Then => for (thenMapping <- special.args(0).asInstanceOf[BlockStatement].asScala) {
          dst.get_after().add(rewrite(thenMapping))
        }
        case _ => ???
      }
      case nonSpecial => dst.get_after().add(rewrite(nonSpecial))
    }

    dst
  }
}
