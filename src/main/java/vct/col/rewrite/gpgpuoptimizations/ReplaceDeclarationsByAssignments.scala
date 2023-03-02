package vct.col.rewrite.gpgpuoptimizations

import vct.col.ast.expr.BindingExpression
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.ParallelBlock
import vct.col.ast.stmt.decl.{Contract, DeclarationStatement, ProgramUnit}
import vct.col.ast.util.{AbstractRewriter, ContractBuilder}
import scala.collection.JavaConverters._

import scala.collection.mutable

case class ReplaceDeclarationsByAssignments(override val source: ProgramUnit) extends AbstractRewriter(source) {

  var declaredVars = mutable.Seq.empty[DeclarationStatement]

  override def visit(s: DeclarationStatement): Unit = {
    declaredVars = declaredVars :+ s
    result = create assignment(create local_name s.name, rewrite(s.init.getOrElse(rewrite(s.`type`).zero)))
  }

  override def visit(e: BindingExpression): Unit = {
    result = copy_rw.rewrite(e)
  }

  override def rewrite(c: Contract): Contract = {
    result = copy_rw.rewrite(c)
    result.asInstanceOf[Contract]
  }

  override def visit(pb: ParallelBlock): Unit = {
    result = create.parallel_block(pb.label, copy_rw.rewrite(pb.contract), copy_rw.rewrite(pb.itersJava), rewrite(pb.block), copy_rw.rewrite(pb.deps))
  }
}
