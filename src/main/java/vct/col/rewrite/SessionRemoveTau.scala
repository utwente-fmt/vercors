package vct.col.rewrite

import vct.col.ast.stmt.decl.{ASTSpecial, ProgramUnit}
import vct.col.ast.util.AbstractRewriter

class SessionRemoveTau(override val source: ProgramUnit) extends AbstractRewriter(null, true) {

  override def visit(s : ASTSpecial) {
    if(s.kind == ASTSpecial.Kind.TauAction) {
      //remove
    } else {
      super.visit(s)
    }
  }
}
