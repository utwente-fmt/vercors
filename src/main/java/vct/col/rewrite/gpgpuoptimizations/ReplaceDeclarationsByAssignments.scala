package vct.col.rewrite.gpgpuoptimizations

import vct.col.ast.stmt.decl.{DeclarationStatement, ProgramUnit}
import vct.col.ast.util.AbstractRewriter

case class ReplaceDeclarationsByAssignments(override val source: ProgramUnit) extends AbstractRewriter(source) {
  override def visit(s: DeclarationStatement): Unit = {
    result = create assignment(create local_name s.name, rewrite(s.init.getOrElse(rewrite(s.`type`).zero)))
  }
}
