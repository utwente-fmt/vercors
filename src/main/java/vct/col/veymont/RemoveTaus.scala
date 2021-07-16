package vct.col.veymont

import vct.col.ast.stmt.decl.{ASTSpecial, ProgramUnit}
import vct.col.ast.util.AbstractRewriter

class RemoveTaus(override val source: ProgramUnit) extends AbstractRewriter(null, true) {

  override def visit(s: ASTSpecial): Unit = {
    if (s.kind == ASTSpecial.Kind.TauAction) {
      //remove
    } else {
      super.visit(s)
    }
  }
}
